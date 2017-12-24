(ns parsatron.test.core
  (:require [parsatron.core :refer [bind always token token=
                                    eof lookahead
                                    never nxt run
                                    attempt maybe choice either many many1 times
                                    ++ -- >> <<
                                    letter digit string whitespace whitespace?
                                    Continue? ->Ok]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-trampoline
  (testing "always is a function"
    (is (fn? (always 5))))

  (testing "with no next parser, always returns Ok"
    (let [p (always 5)
          result (p nil nil nil (fn eok [item _] (->Ok item)) nil)]
      (is (= (->Ok 5) result))))

  (testing "bound to a next parser, always returns Continue"
    (let [p (bind (always 5) (fn [x] (always (+ x 2))))
          p-continue (p nil nil nil (fn eok [item _] (->Ok item)) nil)]
      (is (Continue? p-continue))
      (let [q-continue (p-continue)]
        (is (Continue? q-continue))
        (let [result (q-continue)]
          (is (= (->Ok 7) result)))))))

(deftest test-always
  (is (= 5 (run (always 5) "")))
  (is (= 5 (run (always 5) "abc"))))

(deftest test-nxt
  (is (= 5 (run (nxt (always 3)
                     (always 5)) ""))))

(deftest test-bind
  (is (= 8 (run (bind (always 3)
                      (fn [x]
                        (always (+ x 5)))) ""))))

(deftest test-never
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"parser never succeeds" (run (never) "")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"parser never succeeds" (run (never) "abc"))))

(deftest test-either
  (testing "first parser succeeds"
    (is (= 5 (run (either (always 5) (always 3)) ""))))

  (testing "second parser succeeds, when first fails with empty"
    (is (= 5 (run (either (never) (always 5)) ""))))

  (testing "when neither succeed, errors are combined"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected token 'c', Unexpected token 'c'"
          (run (either (token= \a) (token= \b)) "c")))))

(deftest test-attempt
  (testing "success returns value of p"
    (is (= \a (run (attempt (token= \a)) "a"))))

  (testing "failure is same as never"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected token 'b'" (run (attempt (token= \a)) "b")))
    (is (= \c (run (either (attempt (>> (token= \a) (token= \b)))
                           (>> (token= \a) (token= \c))) "ac")))))

(deftest test-token
  (testing "throws error on empty input"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected end of input"
          (run (token (constantly true)) ""))))

  (testing "consume? determines parser's behavior, error message describes expectation"
    (is (= \a (run (token (constantly true)) "a")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected token 'a'"
          (run (token (constantly false)) "a")))))

(deftest test-many
  (testing "throws an exception if parser does not consume"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Combinator 'many' is applied to a parser that accepts an empty string"
          (run (many (always 5)) ""))))

  (testing "returns empty seq when no input consumed"
    (is (empty? (run (many (token= \a)) ""))))

  (testing "parser returns list of consumed items"
    (let [a-or-b (fn [] (either (token= \a) (token= \b)))]
      (is (= [\a \a \b \a \b \b] (run (many (a-or-b)) "aababbc")))))

  (testing "does not blow the stack"
    (is (= (take 1000 (repeat \a)) (run
                                    (many (token= \a))
                                    (apply str (take 1000 (repeat \a))))))))

(deftest test-times
  (testing "0 times returns nil, and does not consume anything"
    (is (= nil (run (times 0 (token= \a)) ""))))

  (testing "throws an error (from underlying parser) if fewer than specified"
    (let [aaa (fn [] (times 3 (token= \a)))]
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected end of input" (run (aaa) "")))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected end of input" (run (aaa) "a")))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected end of input" (run (aaa) "aa")))))

  (testing "returns a list with the results"
    (is (= [\a \a \a] (run (times 3 (token= \a)) "aaa")))
    (is (= [5 5 5] (run (times 3 (always 5)) ""))))

  (testing "does not blow the stack"
    (is (= (take 10000 (repeat \a)) (run (times 10000 (token= \a))
                                         (apply str (take 10000 (repeat \a))))))))

(deftest test-lookahead
  (testing "returns value of p on success"
    (is (= \a (run (lookahead (token= \a)) "a"))))

  (testing "does not consume input on success"
    (is (= \a (run (>> (lookahead (token= \a)) (token= \a)) "a")))))

(deftest test-choice
  (testing "first parser to succeed returns result"
    (let [a-or-b-or-c (fn [] (choice (token= \a) (token= \b) (token= \c)))]
      (is (= \a (run (a-or-b-or-c) "a")))
      (is (= \b (run (a-or-b-or-c) "b")))
      (is (= \c (run (a-or-b-or-c) "c"))))))

(deftest test-eof
  (testing "parser succeeds, returns nil when no more input left"
    (is (= nil (run (eof) "")))
    (is (= nil (run (>> (token= \a) (eof)) "a"))))

  (testing "parser fails with message when input is left"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Expected end of input" (run (eof) "a")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Expected end of input" (run (>> (token= \a) (eof)) "ab")))))

(deftest test-catchall
  (let [catchall (fn [] (many (token)))]
    (testing "catchall parser succeeds on everything"
      (is (= nil (run (catchall) "")))
      (is (= [\A \b \c] (run (catchall) "Abc")))
      ; input doesn't have to be a string
      (is (= [\A \b \c] (run (catchall) [\A \b \c])))
      ; input doesn't even have to be a sequence of characters
      (is (= [1 [] [2 3]] (run (catchall) [1 [] [2 3]]))))))

(deftest test-multiline
  (let [solid-token (fn [] (token (complement whitespace?)))
        solid-line  (fn [] (<< (many1 (solid-token)) (maybe (token= \newline))))]
    (testing "parser succeeds on matching multiline input"
      (is (= [[\A \b]] (run (<< (many (solid-line)) (eof)) "Ab")))
      (is (= [[\A \b] [\C \d]] (run (<< (many (solid-line)) (eof)) "Ab\nCd")))
      (is (= [[\A \b] [\C \d]] (run (<< (many (solid-line)) (eof)) "Ab\nCd\n"))))
    (testing "parser fails on problematic multiline input"
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Expected end of input"
            (run (<< (many (solid-line)) (eof)) "Ab ")))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Expected end of input"
            (run (<< (many (solid-line)) (eof)) "Ab\n\n"))))))

(deftest test-whitespace
  (let [collapse-space (fn [] (>> (many1 (whitespace)) (always \space)))
        split-on-space (fn [] (many (either (collapse-space) (token))))]
    (is (= [\a \space \b \space \c \space \d] (run (split-on-space) "a\n\nb\t c\t\td")))))

(deftest test-string
  (is (= [\H \e \l \l \o] (run (string "Hello") "Hello world!"))))

(deftest test-var-combiners
  (testing "++ parser captures seq of matches"
    (let [letter1 (fn [] (++ (letter)))]
      (is (= [\a] (run (letter1) "a")))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected token '1'" (run (letter1) "1"))))
    (let [letter-digit (fn [] (++ (letter) (digit)))]
      (is (= [\a \1] (run (letter-digit) "a1")))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected token 'b'" (run (letter-digit) "ab")))))

  (testing "-- parser matches seq but produces nil"
    (let [check-letter-digit (fn [] (-- (letter) (digit)))]
      (is (= nil (run (check-letter-digit) "a1")))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected token 'b'" (run (check-letter-digit) "ab")))))

  (testing ">> parser matches seq but produces only last value"
    (let [letter' (fn [] (>> (letter)))]
      (is (= \a (run (letter') "a"))))
    (let [letter?-digit (fn [] (>> (letter) (digit)))]
      (is (= \1 (run (letter?-digit) "a1")))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected token '1'" (run (letter?-digit) "12")))))

  (testing "<< parser matches seq but produces only first value"
    (let [letter' (fn [] (<< (letter)))]
      (is (= \a (run (letter') "a"))))
    (let [letter-digit? (fn [] (<< (letter) (digit)))]
      (is (= \a (run (letter-digit?) "a1")))
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected token 'b'" (run (letter-digit?) "ab"))))))

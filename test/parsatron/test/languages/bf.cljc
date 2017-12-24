(ns parsatron.test.languages.bf
  (:require [parsatron.core :refer [run]]
            [parsatron.languages.bf :refer [bf]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-accepts-valid-bf
  (is (run (bf) ">"))
  (is (run (bf) "<"))
  (is (run (bf) "+"))
  (is (run (bf) "-"))
  (is (run (bf) "."))
  (is (run (bf) ","))
  (is (run (bf) "[+]"))
  (is (run (bf) ",>++++++[<-------->-],[<+>-]<.")))

(deftest test-rejects-invalid-bf
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Expected end of input" (run (bf) "a")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Expected end of input" (run (bf) "abc")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected end of input" (run (bf) "[+")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Expected end of input" (run (bf) "]")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unexpected end of input" (run (bf) "[+>[+]"))))

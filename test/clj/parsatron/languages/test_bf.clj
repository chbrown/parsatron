(ns parsatron.languages.test-bf
  (:require [clojure.test :refer :all]
            [the.parsatron :refer [run]]
            [parsatron.languages.bf :refer :all]))

(defn valid-bf? [input]
  (try
    (do (run (bf) input) true)
    (catch RuntimeException _ false)
    (catch Exception _ nil)))

(deftest test-accepts-valid-bf
  (is (true? (valid-bf? ">")))
  (is (true? (valid-bf? "<")))
  (is (true? (valid-bf? "+")))
  (is (true? (valid-bf? "-")))
  (is (true? (valid-bf? ".")))
  (is (true? (valid-bf? ",")))
  (is (true? (valid-bf? "[+]")))
  (is (true? (valid-bf? ",>++++++[<-------->-],[<+>-]<."))))

(deftest test-rejects-invalid-bf
  (is (false? (valid-bf? "a")))
  (is (false? (valid-bf? "abc")))
  (is (false? (valid-bf? "[+")))
  (is (false? (valid-bf? "]")))
  (is (false? (valid-bf? "[+>[+]"))))

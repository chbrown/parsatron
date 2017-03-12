(ns parsatron.languages.test-bencode
  (:require [clojure.test :refer :all]
            [the.parsatron :refer [run]]
            [parsatron.languages.bencode :refer :all]))

(deftest test-ben-integer
  (is (= 42 (run (ben-integer) "i42e"))))

(deftest test-ben-bytestring
  (is (= "spam" (run (ben-bytestring) "4:spam"))))

(deftest test-ben-list
  (is (= [42 "spam"] (run (ben-list) "li42e4:spame"))))

(deftest test-ben-dictionary
  (is (= {42 "spam", "spam" 42} (run (ben-dictionary) "di42e4:spam4:spami42ee"))))

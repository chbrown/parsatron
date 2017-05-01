(ns parsatron.languages.bf
  (:refer-clojure :exclude [char])
  (:require [the.parsatron :refer :all]))

(defparser instruction []
  (choice (char \>)
          (char \<)
          (char \+)
          (char \-)
          (char \.)
          (char \,)
          (between (char \[) (char \]) (many (instruction)))))

(defparser bf []
  (many (instruction))
  (eof))

(ns parsatron.languages.bf
  (:require [parsatron.core :refer [<< choice token= between eof many always defparser] :include-macros true]))

(defparser instruction
  []
  (choice (token= \>)
          (token= \<)
          (token= \+)
          (token= \-)
          (token= \.)
          (token= \,)
          (between (token= \[) (token= \]) (many (instruction)))))

(defn bf
  []
  (<< (many (instruction)) (eof)))

(ns parsatron.languages.bencode
  (:require [parsatron.core :refer [>> choice token= always either
                                    many many1 times between
                                    character digit defparser let->>] :include-macros true]))

(defn- parse-int
  "Parse s as an integer"
  [s]
  #?(:clj  (Integer/parseInt s)
     :cljs (let [n (js/parseInt s 10)]
             (when-not (js/isNaN n) n))))

; a bencoded value can be one of: integer, bytestring, list, dictionary
(declare ben-value)

(defn positive-int
  []
  (let->> [digits (many1 (digit))]
    (always (parse-int (apply str digits)))))

(defn negative-int
  []
  (let->> [digits (>> (token= \-) (many1 (digit)))]
    (always (- (parse-int (apply str digits))))))

; An integer is encoded as "i" + <digits> + "e"
(defn ben-integer
  []
  (between (token= \i) (token= \e)
           (either (positive-int)
                   (negative-int))))

; A bytestring (not necessarily unicode characters) is encoded as <length> + ":" + <contents>
(defn ben-bytestring
  []
  (let->> [length (positive-int)
           _ (token= \:)
           chars (times length (character))]
    (always (apply str chars))))

; A list is encoded as "l" + <contents> + "e"
(defn ben-list
  []
  (between (token= \l) (token= \e)
           (many (ben-value))))

; A dictionary is encoded as "d" + <key value pairs> + "e"
(defn ben-dictionary
  []
  (let->> [entries (between (token= \d) (token= \e)
                            (many (times 2 (ben-value))))]
    (always (into (hash-map) (map vec entries)))))

(defparser ben-value
  []
  (choice (ben-integer)
          (ben-bytestring)
          (ben-list)
          (ben-dictionary)))

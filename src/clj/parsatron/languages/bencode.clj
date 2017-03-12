(ns parsatron.languages.bencode
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

; a bencoded value can be one of: integer, bytestring, list, dictionary
(declare ben-value)

(defparser positive-int []
  (let->> [digits (many1 (digit))]
    (always (read-string (apply str digits)))))

(defparser negative-int []
  (let->> [digits (>> (char \-) (many1 (digit)))]
    (always (read-string (apply str digits)))))

; An integer is encoded as "i" + <digits> + "e"
(defparser ben-integer []
  (between (char \i) (char \e)
           (either
            (positive-int)
            (negative-int))))

; A bytestring (not necessarily unicode characters) is encoded as <length> + ":" + <contents>
(defparser ben-bytestring []
  (let->> [length (positive-int)
           _ (char \:)
           chars (times length (any-char))]
    (always (apply str chars))))

; A list is encoded as "l" + <contents> + "e"
(defparser ben-list []
  (between (char \l) (char \e)
           (many (ben-value))))

; A dictionary is encoded as "d" + <key value pairs> + "e"
(defparser ben-dictionary []
  (let->> [entries (between (char \d) (char \e)
                            (many (times 2 (ben-value))))]
    (always (into (hash-map) (map vec entries)))))

(defparser ben-value []
  (choice (ben-integer)
          (ben-bytestring)
          (ben-list)
          (ben-dictionary)))

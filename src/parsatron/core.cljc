(ns parsatron.core)

(defrecord ParseError [state msg])

(defrecord Continue [f]
  #?@(:clj  [clojure.lang.IFn
             (invoke [_] (f))]
      :cljs [IFn
             (-invoke [_] (f))]))
(def Continue? (partial instance? Continue))
(defrecord Ok [value])
(defrecord Err [msg])

(defrecord InputState [input pos])
(defrecord SourcePos [line column])

(defn- inc-sourcepos
  "Increment the source position by a single character, c.
  On newline, increments the SourcePos's line number and resets the column,
  on all other characters, increments the column."
  [{:keys [line column]} c]
  (if (= c \newline)
    (SourcePos. (inc line) 1)
    (SourcePos. line (inc column))))

(defn- sequentially
  [f value]
  (if (instance? Continue value)
    (Continue. #(sequentially f (value)))
    (f value)))

(defn bind
  "Parse p, and then q. The function f must be of one argument,
  it will be given the value of p and must return the q to follow p."
  [p f]
  (fn [state cok cerr eok eerr]
    (let [pcok (fn pcok [item state]
                 (sequentially
                  (fn [q] (Continue. #(q state cok cerr cok cerr)))
                  (f item)))
          peok (fn peok [item state]
                 (sequentially
                  (fn [q] (Continue. #(q state cok cerr eok eerr)))
                  (f item)))]
      (Continue. #(p state pcok cerr peok eerr)))))

(defn always
  "A parser that always succeeds with the value given and consumes no input."
  [value]
  (fn [state _cok _cerr eok _eerr]
    (eok value state)))

(defn never
  "A parser that always fails, consuming no input"
  []
  (fn [state _cok _cerr _eok eerr]
    (eerr (ParseError. state "Error (parser never succeeds)"))))

(defn either
  "A parser that tries p, and on success returns its value,
  but upon failure (if no input was consumed) tries to parse q."
  [p q]
  (fn [state cok cerr eok eerr]
    (let [peerr (fn peerr [err-from-p]
                  (let [qeerr (fn qeerr [err-from-q]
                                ; TODO: combine errors better
                                (let [combined-msg (str (:msg err-from-p) ", " (:msg err-from-q))
                                      combined-err (ParseError. (:state err-from-q) combined-msg)]
                                  (eerr combined-err)))]
                    (Continue. #(q state cok cerr eok qeerr))))]
      (Continue. #(p state cok cerr eok peerr)))))

(defn choice
  "A varargs version of either that tries each given parser in turn,
  returning the value of the first one that succeeds."
  ([p] p)
  ([p & more]
   (either p (apply choice more))))

(defn attempt
  "A parser that will attempt to parse p, and upon failure never consume any input."
  [p]
  (fn [state cok _cerr eok eerr]
    (Continue. #(p state cok eerr eok eerr))))

(defn maybe
  "Attempt to parse p, consuming nothing on failure, but returning nil with success."
  [p]
  (either (attempt p)
          (always nil)))

(defn ++
  "Runs each of the given parsers in sequence, returning a seq of the resulting values."
  ([p] (bind p (fn [value] (always (list value)))))
  ([p & more] (bind p (fn [value]
                        (bind (apply ++ more) (fn [values]
                                                (always (cons value values))))))))

(defn --
  "Runs each of the given parsers in sequence, returning nil."
  ([p] (bind p (fn [_value] (always nil))))
  ([p & more] (bind p (fn [_value] (apply -- more)))))

(defn >>
  "Runs each of the parsers in sequence, returning only the last result."
  ([p] p)
  ([p & more] (bind p (constantly (apply >> more)))))

(defn ^:deprecated nxt
  "Deprecated. Use >> instead.
  Parse p and then q, returning q's value and discarding p's."
  [p q]
  (>> p q))

(defn <<
  "Run each of the parsers in sequence, returning the result of the first parser in the sequence."
  ([p] p)
  ([p & more] (bind p (fn [value]
                        (bind (apply -- more) (fn [_]
                                                (always value)))))))

(defn token
  "Consume the first value from input. Fails if the input is empty.
  If `consume?` is provided, fail if (consume? value) is not true."
  ([]
   (token (constantly true)))
  ([consume?]
   {:pre [(ifn? consume?)]}
   (fn [{:keys [input pos] :as state} cok _cerr _eok eerr]
     (if-let [tok (first input)]
       (if (consume? tok)
         (cok tok (InputState. (rest input) (inc-sourcepos pos tok)))
         (eerr (ParseError. state (str "Unexpected token '" tok "'"))))
       (eerr (ParseError. state "Unexpected end of input"))))))

(defn token=
  "Matches a token exactly equal to `value`"
  [value]
  (token (partial = value)))

(defn- unchecked-many
  [p]
  (either (bind p (fn [value]
                    (bind (unchecked-many p) (fn [values]
                                               (always (cons value values))))))
          (always nil)))

(defn many
  "Consume zero or more p. Results in nil if there are zero matches.
  An exception will be thrown if this combinator is applied to a parser that
  accepts the empty string, as that would cause the parser to loop forever."
  [p]
  (let [eok-error (fn eok-error [_item _state]
                    (ParseError. nil "Combinator 'many' is applied to a parser that accepts an empty string"))
        safe-p    (fn safe-p [state cok cerr _eok eerr]
                    (Continue. #(p state cok cerr eok-error eerr)))]
    (unchecked-many safe-p)))

(defn many1
  "Consume one or more p.
  Like many, this will throw an exception if p matches the empty string."
  [p]
  (bind p (fn [value]
            (bind (many p) (fn [values]
                             (always (cons value values)))))))

(defn times
  "Consume a sequence of exactly `n` matches of the parser `p`"
  [n p]
  (if (pos? n)
    (bind p (fn [value]
              (bind (times (dec n) p) (fn [values]
                                        (always (cons value values))))))
    (always nil)))

(defn lookahead
  "A parser that upon success consumes no input, but returns what was parsed."
  [p]
  (fn [state _cok cerr eok eerr]
    (let [ok (fn ok [item _state]
               (eok item state))]
      (Continue. #(p state ok cerr eok eerr)))))

(defn eof
  "A parser to detect the end of input.
  If there is nothing more to consume from the underlying input,
  this parser suceeds with a nil value, otherwise it fails."
  []
  (fn [state _cok _cerr eok eerr]
    (if (empty? (:input state))
      (eok nil state)
      (eerr (ParseError. state "Expected end of input")))))

(defn character
  "Consume any character"
  []
  (token char?))

(defn digit?
  "Tests if a character is a digit: [0-9]"
  [c]
  #?(:clj  (Character/isDigit ^char c)
     :cljs (re-matches #"[0-9]" c)))

(defn digit
  "Consume a digit [0-9] character"
  []
  (token digit?))

(defn letter?
  "Tests if a character is a letter: [a-zA-Z]"
  [c]
  #?(:clj  (Character/isLetter ^char c)
     :cljs (re-matches #"[a-zA-Z]" c)))

(defn letter
  "Consume a letter [a-zA-Z] character"
  []
  (token letter?))

(defn whitespace?
  "Tests if a character is whitespace: \\s"
  [c]
  #?(:clj  (Character/isWhitespace ^char c)
     :cljs (re-matches #"\s" c)))

(defn whitespace
  "Consume a whitespace character"
  []
  (token whitespace?))

(defn string
  "Consume the given string"
  [s]
  (apply ++ (map token= s)))

(defn between
  "Parse p after parsing open and before parsing close,
  returning the value of p and discarding the values of open and close."
  [open close p]
  (bind open (fn [_openvalue]
               (bind p (fn [value]
                         (bind close (fn [_closevalue]
                                       (always value))))))))

(defn- itrampoline
  "Identical to clojure.core/trampoline, but checks for ifn? rather than fn?"
  ([f]
   (let [ret (f)]
     (if (ifn? ret)
       (recur ret)
       ret)))
  ([f & args]
   (itrampoline #(apply f args))))

(defn- show-error
  [{:keys [state msg] :as parse-error}]
  (let [{:keys [pos]} state
        {:keys [line column]} pos]
    (str msg " at line: " line " column: " column)))

(defn- run-parser
  "Execute a parser p, given some state, Returns Ok or Err."
  [p state]
  (itrampoline p state
               (fn cok [item _state]
                 (Ok. item))
               (fn cerr [err]
                 (Err. (show-error err)))
               (fn eok [item _state]
                 (Ok. item))
               (fn eerr [err]
                 (Err. (show-error err)))))

(defn run
  "Run a parser p over some input. The input can be a string or a seq of tokens,
  if the parser produces an error, its message is wrapped in a ExceptionInfo
  and thrown, and if the parser succeeds, its value is returned."
  [p input]
  (let [result (run-parser p (InputState. input (SourcePos. 1 1)))]
    (if (instance? Ok result)
      (:value result)
      ; presumably, result is an Err instance
      (throw (ex-info (:msg result) {:todo :something})))))

;; Macros are for user convenience; they are not used interally, so they appear last

(defmacro defparser
  "Defines a new parser. Parsers are simply functions that accept the
  5 arguments state, cok, cerr, eok, eerr but this macro takes care
  of writing that ceremony for you and wraps the body in a >>"
  [name args & body]
  `(defn ~name ~args
     (fn [state# cok# cerr# eok# eerr#]
       (let [p# (>> ~@body)]
         (Continue. #(p# state# cok# cerr# eok# eerr#))))))

(defmacro let->>
  "Expands into nested bind forms"
  [[& bindings] & body]
  (let [[bind-form p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(bind ~p (fn [~bind-form] ~@body))
      `(bind ~p (fn [~bind-form] (let->> ~(drop 2 bindings) ~@body))))))

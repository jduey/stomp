(ns stomp.parser
  (:require [monads.core :as m]
            [monads.macros :as mm]))

(deftype parser-m [v mv f]
  clojure.lang.IFn
  (invoke [_ s]
    (if f
      (if-let [[v ss] (mv s)]
        ((f v) ss)
        nil)
      [v s]))

  m/Monad
  (do-result [_ v]
    (parser-m. v nil nil))
  (bind [mv f]
    (parser-m. nil mv f))

  m/MonadZero
  (zero [_]
    (constantly nil))
  (plus-step [mv mvs]
    (parser-m. nil
               (fn [s]
                 (loop [[mv & mvs] (cons mv mvs)]
                   (when mv
                     (if-let [result (mv s)]
                       result
                       (recur mvs)))))
               (fn [v] (parser-m. v nil nil)))))

(defn parser [v]
  (parser-m. v nil nil))

(defmacro p-do [bindings expr]
  `(monads.macros/do stomp.parser/parser ~bindings ~expr))

(defn optional [p]
  (m/plus [p (parser nil)]))

(declare one-or-more)

(defn none-or-more [p]
  (optional (one-or-more p)))

(defn one-or-more [p]
  (p-do
   [a p
    as (none-or-more p)]
   (cons a as)))

(def next-char
  (reify
    m/Monad
    (do-result [_ v]
      (parser v))
    (bind [mv f]
      (fn [text]
        (when (> (count text) 0)
          ((f (first text)) (subs text 1)))))))

(defn char-test [pred]
  (p-do
   [c next-char
    :when (pred c)]
   c))

(defn is-char [c]
  (char-test #(= c %)))

(defn one-of [coll]
  (char-test (set coll)))

(def alpha (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def digit (one-of "0123456789"))

;; TODO: Make faster
(defn is-string [s]
  (if (empty? s)
    (parser s)
    (p-do
     [_ (is-char (first s))
      _ (is-string (subs s 1))]
     s)))

(def NULL (is-char \0))
(def LF (is-char \newline))
(def CR (is-char 13))
(def EOL (m/seq [(optional CR) LF]))

(def non-terminator
  (char-test #(condp = %
                \0 false
                \newline false
                13 false
                \: false
                true)
   ))

(def octet
  (char-test #(not= % \0)))

(def header-value
  (none-or-more non-terminator))

(def header-name
  (one-or-more non-terminator))

(def header
  (p-do
   [name header-name
    _ (is-char \:)
    val header-value]
   {(apply str name)
    (apply str val)}))

(def server-command
  (m/plus [(is-string "CONNECTED")
           (is-string "MESSAGE")
           (is-string "RECEIPT")
           (is-string "ERROR")]))

(def client-command
  (m/plus [(is-string "SUBSCRIBE")
           (is-string "UNSUBSCRIBE")
           (is-string "BEGIN")
           (is-string "COMMIT")
           (is-string "ABORT")
           (is-string "ACK")
           (is-string "NACK")
           (is-string "DISCONNECT")
           (is-string "CONNECT")
           (is-string "STOMP")
           (is-string "SEND")]))

(def command
  (m/plus [server-command
           client-command]))

(def frame
  (p-do
   [cmd command
    _ EOL
    headers (none-or-more header)
    _ EOL
    body (none-or-more octet)
    _ NULL
    ;; _ (none-or-more EOL)
    ]
   {:command cmd
    :headers (apply merge headers)
    :body (apply str body)}))

(def frame-stream
  (one-or-more frame))

(frame-stream
 (str
  "CONNECTED
header:value
body1
body2
body3"
  \0
  "MESSAGE
header:value
message body"
  \0))

(ns jenerator.literals
  (:require [jenerator.util :as u]
            [clojure.string :as s]))

(def valid-integer-bases #{:dec :hex :oct :bin})

(defn- integer->string [number base]
  (condp = base
    :dec (format "%d" number)
    :hex (str "0x" (format "%x" number))
    :oct (str "0" (format "%o" number))
    :bin (str "0b" (Long/toBinaryString number))
    :else (u/error "Illegal integer base: " base))) 

(defn jenerate-integer-literal
  "{Key: Any} -> String
   Convert map of integer literal AST to string.
   Integer literal AST:
     {:jenerate :int
      :value integer
      :base (:dec | :hex | :oct | :bin)
      :long (true | false)}
   Key `:value` (0 by default). Its value is formatted into a string using `:base`.
   Base must be one of the `valid-integer-bases` (:dec by default).
   Key `:long` is optional and chcked only for truethyness.
   Generated strin may start from `0b` or `0x` or `0` or `non-zero integer`."
  [{:keys [base value long] :or {base :dec value 0}}]
  {:pre [(integer? value) (valid-integer-bases base)]}
  (str (integer->string value base) (if long "L" "")))

(defn jenerate-float-literal
  "{Key: Any} -> String
   Convert map of float literal AST to string.
   Float literal AST:
     {:jenerate :float
      :whole integer
      :fraction integer
      :shift integer
      :exponent integer
      :suffix (:float|:double)}
   All keys are optional. Whole and fractional are zeros by default.
   Shift is the number of zeros between the decimal point and fraction.
   Result string is formatted as: '<whole>.<shift><fraction>e<exponent>[F|D]'"
  [{:keys [whole fraction shift exponent suffix] :or {whole 0 fraction 0 shift 0}}]
  {:pre [(integer? whole) (integer? fraction)
         (integer? shift) (>= shift 0)
         (or (nil? exponent) (integer? exponent))
         (or (nil? suffix) (keyword? suffix))]}
  (let [shift-str (apply str (take shift (repeat "0")))
        exponent-str (if (not= (or exponent 0) 0) (str "e" exponent) "")
        neg (if (or (neg? whole) (neg? fraction)) "-" "")
        suff (if suffix (name suffix) "")]
    (str neg (u/abs whole) "." shift-str (u/abs fraction) exponent-str suff)))

(def string-escape-chars
  {\backspace "\\b"
     \tab "\\t"
     \newline "\\n"
     \formfeed "\\f"
     \return "\\r"
     \" "\\\""
     \\ "\\\\"})

(defn jenerate-string-literal
  "; String -> String
   Convert a string into escaped Java string literal."
  [s] (str "\"" (s/escape s string-escape-chars) "\""))

(def char-escape-chars
  (assoc string-escape-chars \' "\\'"))

(defn jenerate-char-literal
  "; Char -> String
   Convert a char into escaped Java char literal"
  [c] (str "'" (char-escape-chars c c) "'"))
(ns jenerator.literals
  (:require [jenerator.util :as u]))

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
      :exponent integer}
   All keys are optional. Whole and fractional are zeros by default.
   Result string is formatted as: '<whole>.<fraction>e<exponent>'"
  [{:keys [whole fraction exponent] :or {whole 0 fraction 0}}]
  {:pre [(integer? whole) (integer? fraction) (or (nil? exponent) (integer? exponent))]}
  (str whole "." fraction (if exponent (str "e" exponent) "")))
  
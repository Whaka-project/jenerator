(ns jenerator.fns
  (:refer-clojure :exclude [int long float double type cast not]))

(defn int
  ([value] {:jtag :int :value value})
  ([value base] {:jtag :int :value value :base base}))

(defn long
  ([value] {:jtag :int :value value :long true})
  ([value base] {:jtag :int :value value :base base :long true}))

(defn- float*
  ([suffix whole]
    {:jtag :float :whole whole :suffix suffix})
  ([suffix whole fraction {:keys [e shift] :or {e 0 shift 0}}]
    {:jtag :float :whole whole :fraction fraction :exponent e :shift shift :suffix suffix}))

(defn float
  ([whole] (float* :f whole))
  ([whole fraction & {:keys [e shift] :or {e 0 shift 0} :as map}] (float* :f whole fraction map)))

(defn double
  ([whole] (float* :d whole))
  ([whole fraction & {:keys [e shift] :or {e 0 shift 0} :as map}] (float* :d whole fraction map)))

(defn ann
  ([class] (ann class :args {}))
  ([class value] (ann class :value value))
  ([class arg-key arg-value]
    (let [map (if (and (= :args arg-key) (map? arg-value)) arg-value {arg-key arg-value})]
      {:jtag :annotation :class class :args map}))
  ([class arg-key arg-value & {:as other-args}]
    (ann class :args (assoc other-args arg-key arg-value))))

(defn type
  ([types] (type types 0))
  ([types array]
    (let [ftype (if (sequential? types) (first types) types)
          generics (if (sequential? types) 
                     (map #(if (or (class? %) (sequential? %)) (type %) %) (rest types)) nil)]
      {:jtag :type :type ftype :generics generics :array array})))

(defn cast
  "; Any -> (AST | Class | [Class])* -> AST
   Function takes a value and and any number of 'type descriptors'.
   Type descriptor is either a type AST map, or anything that may be passed
   to the 'fns/type' fnction as argument.
   Returns - nested AST maps of casting the value to all specified types
   consequentially (last type will be the final result of the cast)."
  [value & types]
  (reduce (fn [val type]
            {:jtag :cast :value val
             :type (if (map? type) type
                     (jenerator.fns/type type))})
          value types))

(defn var
  "; {Key -> Any}* -> Key* -> Class -> String -> {Key -> Any}
   Weird varargs at the beginning of the argument-list denote
   that function actually expects only 2 last arguments - other are optional.
   Function takes 0+ annotation AST-maps, then 0+ modifier keywords,
   then single Class, and then a single string name - in that order.
   Result - AST-map for a var declaration.

   Examples:
     (var Long \"var\")
     (var :final Long \"var\")
     (var (ann Deprecated) :private :final Long \"var\")"
  [& rest]
  {:pre [(>= (count rest) 2)]}
  (let [[name type & stuff] (reverse rest)
        [modifiers annotations] (split-with keyword? stuff)]
    {:jtag :var :annotations (reverse annotations)
     :modifiers (reverse modifiers) :type type :name name}))

(defn decl
  "; Var-AST -> Any -> ([String, Any]) -> Decl-AST
   Function takes a var AST map, a value, and optional seq of pairs of additional declarations.
   Returns AS map describing a variable declaration with initializers.
   If first argument is not a map - it's assumed that it's a seq of values,
   and function 'fns/var' will be applied to it.

   Exaples:
     (fns/decl (fns/var String \"s\") nil)
     (fns/decl [String \"s\"] nil)
     (fns/decl [:int \"i\"] 12 [[\"n\" 13] [\"k\" 14]])
     (fns/decl [:int \"i\"] 12 {\"n\" 13 \"k\" 14})

   It is decided not to have 'rest' args destructured as a map,
   so it would be more convinient for callers to generate calls programatically."
  ([var value] (decl var value nil))
  ([var value values]
   (let [var-ast (if (map? var) var (apply jenerator.fns/var var))]
     {:jtag :decl :var var-ast :value value :values values})))

(defn unary
  "; Any -> Any -> (UnaryPrefix-AST | UnaryPostfix-AST)
   Takes two argument. Either one MUST be a symbol.
   The one that's a symbol will be considered an operation.
   Another one will be considered a value.
   If two symbols is passed - first one considered an operation.
   Returns UnaryPrefix-AST if first argument is the operation.
   Returns UnaryPostfix-AST in other case."
  [a b]
  {:pre [(or (symbol? a) (symbol? b))]}
  (let [[jtag op value] (if (symbol? a)
                          [:pre a b]
                          [:post b a])]
    {:jtag jtag :value value :op op}))

(comment
; This function is commented out, since it must wrap specified value in brackets.
; No brackets AST is available yet.
(defn not
  "; Any -> UnaryPrefix-AST
   Takes a single value and produces AST for unary prefix negation (!)."
  [x]
  (unary '! x))
)

; Should `bin` also jenerate brackets?
(defn bin
  "; Any -> Symbol -> Any (-> Symbol -> Any)* -> Binary-AST
   Takes two or more elenets SEPARATED by operation symbols.
   Returns AST map representing multiple nested binary operations.
   Examples:
     (bin 12 '+ 13)
     (bin 12 '+ 13 '- 14)
     (bin 12 '+ 13 '- 14 '* 15)"
  ([a op b] {:jtag :bin :left a :op op :right b})
  ([a op b & rest]
    {:pre [(even? (count rest))]}
    (apply bin (list* (bin a op b) rest))))

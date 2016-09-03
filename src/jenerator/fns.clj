(ns jenerator.fns
  (:refer-clojure :exclude [int long float double type cast not])
  (:require [jenerator.util :as u]))

(defn- apply-if-v
  "; (T* -> R) -> (A | Vec[T]) -> (A | R)
   Takes a function and any element.
   If element is a vector - applies the function to it and returns the result.
   Otherwise returns the specified element.

   Examples:
     (apply-if-v inc 12) -> 12
     (apply-if-v inc [12]) -> 13

   This function is used by sugary-fns that allow the DSL-like approach to perform
   complex or recursive calls. E.g. there's 'f' that may be called like this: (f 12 13)
   So (f 12 [13 14]) may be equal to (f 12 (f 13 14))"
  [f x]
  (if (vector? x) (apply f x) x))

(defn br
  "; Any -> Brackets-AST
   Takes any value and produces an AST map for brackets expression.
   Example: (br 12)"
  [value]
  {:jtag :br :value value})

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

(declare type-mapper)

(defn type
  "; (Class | Key) -> Type-AST
   ; (Class | Key) -> Long -> Type-AST
   Takes a type identifier (clall or a primitive type key) and optional array type depth.
   Returns AST map representing a type descriptor.

   Examples:
     (type :int) -> AST for int type
     (type String) -> AST for String type
     (type String 2) -> AST for String[][] array type
     (type Class) -> AST for raw Class type
     (type [Class]) -> AST for diamond Class<> type
     (type [Class Long]) -> AST for Class<Long> type
     (type [Class [Class Long]] 1) -> AST for Class<Class<Long>>[] type

   Vector calls are recursive, but array depth cannot ve specified that way.
   So if you want to create a type with generic of array type, you have to use nested calls.
   Example: (type [Class (type :long 1)]) -> AST for Class<long[]> type"
  ([types] (type types 0))
  ([types array]
    (let [is-vec (vector? types)
          ftype (if is-vec (first types) types)
          generics (if is-vec (map type-mapper (rest types)) nil)]
      {:jtag :type :type ftype :generics generics :array array})))

(def type-identifier?
  "; Any -> Boolean
   Returns true if specified value may represent a type in the DSL."
  (u/anyp keyword? class? vector?))

(defn- type-mapper
  "; (T | Type-Identifier) -> (T | Type-AST)
   Takes any value, checks whether it's a `type-identifier',
   and either applies `type' function to it, or returns value itself."
  [value]
  (if (type-identifier? value)
    (type value) value))

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
             :type (type-mapper type)})
          value types))

(defn var
  "; {Key -> Any}* -> Key* -> Type-Identifier -> String -> {Key -> Any}
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
     :modifiers (reverse modifiers) :type (type-mapper type) :name name}))

(defn decl
  "; Var-AST -> Any -> Decl-AST
   ; Var-AST -> Any -> ([String, Any]) -> Decl-AST
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
   (let [var-ast (apply-if-v jenerator.fns/var var)]
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

(defn not
  "; Any -> UnaryPrefix-AST
   Takes a single value and produces AST for unary prefix negation (!)."
  [x]
  (unary '! (br x)))

(defn bin
  "; Any -> Symbol -> Any -> Binary-AST
   Takes left operand, operation symbol, and right operand.
   Returns AST map representing multiple nested binary operations.
   Example: (bin 12 '+ 13)

   If an operand is a vector - `bin` is also applied to it.
   Example: (bin [12 '+ 13] '* 14) == (bin (bin 12 '+ 13) '* 14)"
  [left op right]
  (let [bin-br (comp br bin)]
    {:jtag :bin
     :left (apply-if-v bin-br left)
     :right (apply-if-v bin-br right)
     :op op}))

(defn field
  "; Any -> String -> FieldRef-AST
   Takes any target and string field name.
   Returns an AST map that represents a field access operation.
   Example: (field Integer \"MAX_VALUE\")"
  [target field]
  {:jtag :field :target target :field field})

(defn clref
  "; Any -> FieldRef-AST
   Takes any target.
   Returns an AST map that represents a '.class' field access.
   Example: (clref String) = (field String \"class\")"
  [target]
  (field target "class"))

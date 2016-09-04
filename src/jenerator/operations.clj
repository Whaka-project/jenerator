(ns jenerator.operations
  (:require [jenerator.util :as u]))

(defn- check-as-statement
  [data res]
  (if (-> data meta :as-statement)
    (str res ";") res))

(defn jenerate-prefix
  "; (Any -> String) -> UnaryPrefix-AST -> String
   Takes a jen function and an AST map for unary prefix operation.
   Returns Java source code string.

   UnaryPrefix-AST:
     :value - Any
     :op - Symbol

   Value will also be jenerated.
   Operation may be represented only as a symbol."
  [jen-fn {:keys [value op] :as data}]
  {:pre [(symbol? op)]}
  (check-as-statement data
    (str op (jen-fn value))))

(defn jenerate-postfix
  "; (Any -> String) -> UnaryPostfix-AST -> String
   Takes a jen function and an AST map for unary postfix operation.
   Returns Java source code string.

   UnaryPostfix-AST:
     :value - Any
     :op - Symbol

   Value will also be jeneated.
   Operation may be represented only as a symbol."
  [jen-fn {:keys [value op] :as data}]
  {:pre [(symbol? op)]}
  (check-as-statement data
    (str (jen-fn value) op)))

(defn jenerate-binary
  "; (Any -> String) -> Binary-AST -> String
   Takes a jen function and an AST map for binary operation.
   Returns Java source code string.

   Binary-AST:
     :left - Any
     :right - Any
     :op - Symbol

   Left and right operands will also be jenerated.
   Operation may be represented only as a symbol."
  [jen-fn {:keys [left right op] :as data}]
  {:pre [(symbol? op)]}
  (check-as-statement data
    (str (jen-fn left) " " op " " (jen-fn right))))

(defn jenerate-brackets
  "; (Any -> String) -> Brackets-AST -> String
   Takes a jen function and an AST map for brackets expression.
   Returns Java source code string.

   Brackets-AST:
     :value - Any

   Value s also jenerated.
   Note value MUST be present in the map, or excepton will be thrown."
  [jen-fn {:keys [value] :or {value ::none}}]
  {:pre [(not= value ::none)]}
  (str \( (jen-fn value) \)))

(defn jenerate-field-access
  "; (Any -> String) -> FieldRef-AST -> String
   Takes a jen function and an AST map for field access.
   Returns Java source code string.

   FieldRef-AST:
     :target - Any
     :field - String

   Target is also jenerated."
  [jen-fn {:keys [target field]}]
  {:pre [(string? field)]}
  (str (jen-fn target) "." field))

(defn jenerate-method-call
  "; (Any -> String) -> MethodCall-AST -> String
   Takes a jen function and an AST map for method call.
   Returns Java source code string.

   MethodCall-AST:
     :target - Any
     :method - String
     :args - [Any]

   Target and each arg are also jenerated."
  [jen-fn {:keys [target method args] :as data}]
  {:pre [(string? method) (or (nil? args) (sequential? args))]}
  (check-as-statement data
    (str (jen-fn target) "." method (->> args (map jen-fn) u/jn-args))))

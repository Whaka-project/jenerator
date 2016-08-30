(ns jenerator.operations)

(defn jenerate-prefix
  "; (Any -> String) -> UnaryPrefix-AST -> String
   Takes a jen function and an AST map for unary prefix operation.
   Returns Java source code string.

   UnaryPrefix-AST:
     :value - Any
     :op - Symbol

   Value will also be jenerated.
   Operation may be represented only as a symbol."
  [jen-fn {:keys [value op]}]
  {:pre [(symbol? op)]}
  (str op (jen-fn value)))

(defn jenerate-postfix
  "; (Any -> String) -> UnaryPostfix-AST -> String
   Takes a jen function and an AST map for unary postfix operation.
   Returns Java source code string.

   UnaryPostfix-AST:
     :value - Any
     :op - Symbol

   Value will also be jeneated.
   Operation may be represented only as a symbol."
  [jen-fn {:keys [value op]}]
  {:pre [(symbol? op)]}
  (str (jen-fn value) op))

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
  [jen-fn {:keys [left right op]}]
  {:pre [(symbol? op)]}
  (str (jen-fn left) " " op " " (jen-fn right)))

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

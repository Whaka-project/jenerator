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
     :valie - Any
     :op - Symbol

   Value will also be jeneated.
   Operation may be represented only as a symbol."
  [jen-fn {:keys [value op]}]
  {:pre [(symbol? op)]}
  (str (jen-fn value) op))

(defn jenerate-binary
  [jen-fn {:keys [left right op]}]
  {:pre [(symbol? op)]}
  (str (jen-fn left) " " op " " (jen-fn right)))

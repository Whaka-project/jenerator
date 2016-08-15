(ns jenerator.operations)

(defn jenerate-prefix
  [jen-fn {:keys [value op]}]
  {:pre [(symbol? op)]}
  (str op (jen-fn value)))

(defn jenerate-postfix
  [jen-fn {:keys [value op]}]
  {:pre [(symbol? op)]}
  (str (jen-fn value) op))

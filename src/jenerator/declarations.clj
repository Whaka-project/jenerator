(ns jenerator.declarations
  (:require [jenerator.util :as u]))

(defn jenerate-var
  "; (Any -> String) -> {Key -> Any} -> String
   Function takes a jen function and AST-map for variable declaration.
   Produces string Java source.

   AST map may contain:
    - seq of annotation AST-maps
    - seq of modifier keywords
    - single type AST-map (or value)
    - single string name

   Annotations and type are also jenerated.
   Modifiers are not validated, but must be keywords.
   Name is not validated, so be sure to produce valid Java identifier."
  [jen-fn {:keys [annotations modifiers type name]}]
  {:pre [(string? name) (every? keyword? modifiers) (not (nil? type))]}
  (let [jen-annotatons (map jen-fn annotations)
        jen-modifiers (map clojure.core/name modifiers)
        jen-type (jen-fn type)
        join #(if (empty? %) "" ((u/joiner " " "" " ") %))]
    (str (join jen-annotatons) (join jen-modifiers) jen-type " " name)))

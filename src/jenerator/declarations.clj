(ns jenerator.declarations
  (:require [jenerator.util :as u]))

(defn jenerate-var
  [jen-fn {:keys [annotations modifiers type name]}]
  {:pre [(string? name) (every? keyword? modifiers) (not (nil? type))]}
  (let [jen-annotatons (map jen-fn annotations)
        jen-modifiers (map clojure.core/name modifiers)
        jen-type (jen-fn type)
        join #(if (empty? %) "" ((u/joiner " " "" " ") %))]
    (str (join jen-annotatons) (join jen-modifiers) jen-type " " name)))

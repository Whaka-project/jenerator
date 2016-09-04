(ns jenerator.statements
  (:require [clojure.string :as s]
            [jenerator.util :as u]))

(defn jenerate-code-block
  "; (Any -> String) -> Block-AST -> String
   Takes a jen function and AST map for block statement.
   Returns a Java source strin.

   Block-AST:
     :statements - [Any]

   Each staement is also jenerated with ':as-statement' meta value.
   Output string is formatted with tabs."
  [jen-fn {:keys [statements] :as data}]
  (if (empty? statements) "{}"
   (let [jen-statement (fn[st] (jen-fn (vary-meta st assoc :as-statement true)))
         jenerated-statements (map jen-statement statements)
         combined-statements (apply u/str-lines jenerated-statements)
         lines (s/split-lines combined-statements)
         tabbed-lines (map #(str \tab %) lines)
         combined-tabbed-lines (apply u/str-lines tabbed-lines)]
     (str "{" \newline combined-tabbed-lines \newline "}"))))

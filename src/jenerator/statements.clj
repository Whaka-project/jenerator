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
  (if (empty? statements) "{\n}"
   (let [jen-statement (fn[st] (jen-fn (vary-meta st assoc :as-statement true)))
         jenerated-statements (map jen-statement statements)
         combined-statements (apply u/str-lines jenerated-statements)
         lines (s/split-lines combined-statements)
         tabbed-lines (map #(str \tab %) lines)
         combined-tabbed-lines (apply u/str-lines tabbed-lines)]
     (str "{\n" combined-tabbed-lines "\n}"))))

(defn- jtag?
  "Returns true is second argument is a map,
   and contains first argument under ':jtag' key."
  [jtag data]
  (and (associative? data)
       (= (:jtag data) jtag)))

(def ^:private block?
  "Returns true if specified data is a Block-AST map"
  (partial jtag? :block))

(def ^:private if?
  "Returns true if specified data is an IfThenElse-AST"
  (partial jtag? :if))

(defn- ensure-block
  "If specified data is a Block-AST map - returns it unchanged.
   Otherwise - wraps it into a block AST and returns it."
  [data]
  (if (block? data) data
    {:jtag :block :statements (if (nil? data) [] [data])}))

(defn jenerate-if
  "; (Any -> String) -> IfThenElse-AST -> String
   Takes a jen function and an AST map for 'if-then-else' statement.
   Returns Java source string.

   IfThenElse-AST:
     :cond - Any (required)
     :then - Any (required, non-nil)
     :else - Any

   All elements are also jenerated.
   If 'then' and 'else' elements are not Block-ASTs - they are wrapped.
   If 'else' branch if IfThenElse-AST - excessive braces are omited."
  [jen-fn {:keys [cond then else] :as data}]
  {:pre [(contains? data :cond) (some? then)]}
  (let [header-str (str "if (" (jen-fn cond) ") ")
        then-str (jen-fn (ensure-block then))
        else-str (if (nil? else) ""
                   (str \newline "else "
                     (jen-fn (if (if? else) else
                               (ensure-block else)))))]
    (str header-str then-str else-str)))

(defn jenerate-for
  [jen-fn {:keys [decl test iters body]}]
  {:pre [(or (nil? iters) (map? iters) (every? some? iters))]}
  (let [decl-str (if (nil? decl) ";" (jen-fn decl))
        test-str (if (nil? test) "" (jen-fn test))
        iters (if (map? iters) [iters] iters)
        iter-str (->> iters (map jen-fn) u/jn-comma)
        body-str (jen-fn (ensure-block body))]
    (str "for (" decl-str " " test-str "; " iter-str ") " body-str)))

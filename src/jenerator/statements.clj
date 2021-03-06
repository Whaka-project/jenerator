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
  "; (Any -> String) -> For-AST -> String
   Takes a jen function and an AST map for 'for' statement.
   Retrns Java source string.

   For-AST:
     :decl - Any
     :test - Any
     :iters - (Map | [Map])
     :body - Any

   Jenerated statements will have the form:
     for (decl; test; iter[, iters]) { body }

   Any element may be nil - in this case corresponding element ill be missing.
   (E.g. no declaration or a predicate, which is totally valid in the 'for'.)

   ':iters' may be either a map (single AST element) or a seqable collection,
   of multiple elements - in this case multiple iteration actions wil be jenerated.

   In case of an empty body - result 'for' will have an empty {} brackets."
  [jen-fn {:keys [decl test iters body]}]
  {:pre [(or (nil? iters) (map? iters) (every? some? iters))]}
  (let [decl-str (if (nil? decl) ";" (jen-fn decl))
        test-str (if (nil? test) "" (jen-fn test))
        iters (if (map? iters) [iters] iters)
        iter-str (->> iters (map jen-fn) u/jn-comma)
        body-str (jen-fn (ensure-block body))]
    (str "for (" decl-str " " test-str "; " iter-str ") " body-str)))

(defn jenerate-label
  "; (Any -> String) -> Label-AST -> String
   Takes a jen function, and an AST map for a label statement.
   Returns Java source code string.

   Label-AST:
     :name - String
     :statement - Any

   Name is jenerated into label.
   Statemen is jenerated recursively."
  [jen-fn {:keys [name statement] :as data}]
  {:pre [(string? name) (some? statement)]}
  (let [is-statement (-> data meta :as-statement)
        statement (if is-statement
                    (vary-meta statement assoc :as-statement is-statement)
                    statement)]
    (str name ": " (jen-fn statement))))

(defn jenerate-branch
  "; (Any -> String) -> Branch-AST -> String
   Takes a jen function, and an AST map for a branch statement.
   Returns Java source code string.

   Branch-AST:
     :mode - Keyword
     :target - Any

   ':mode' is one of the: #{:break, :continue, :return}
   If mode is ':return' - target gets jenerated (if present).
   Otherwise target expected to be a string, or a symbol."
  [jen-fn {:keys [mode target] :or {target ::none} :as data}]
  {:pre [(keyword? mode)]}
  (if-not (:skip-check? data)
    (assert (#{:return :break :continue} mode)))
  (let [force-jen (or (= mode :return) (:force-jen data))
        is-target (not= target ::none)
        target (if (and force-jen is-target) (jen-fn target) target)]
    (if is-target
      (str (name mode) " " target ";")
      (str (name mode) ";"))))

(defn- case-entry->block
  "Takes a jen-function and a pair of: [case element, result code]
   Returns a 'Label-AST' map that renders a Java's case-clause,
   where the 'label' is just a 'case' with a jenerated cond-value."
  [jen-fn [cond code]]
  {:jtag :label
   :name (if (= cond :def)
           "default"
           (str "case " (jen-fn cond)))
   :statement (ensure-block code)})

(defn jenerate-switch
  "; (Any -> String) -> Switch-AST -> String
   Takes a jen-function, and an AST map for the switch statement.
   Returns Java source code string.

   Switch-AST:
     :target - Any
     :cases - [[(Any | :def), Any]]

   Target is also jenerated. Cases is a sequence of tuples,
   where each first element is a case-cond (or the ':def' key
   for the default clause), and second element is the case's code.
   Note: code is wrapped in a block-sttement, so only sttement ASTs will work."
  [jen-fn {:keys [target cases]}]
  {:pre [(some? target) (and (sequential? cases)
                             (not-empty cases)
                             (every? sequential? cases)
                             (every? #(= (count %) 2) cases))]}
  (let [target-str (jen-fn target)
        case-blocks (map #(case-entry->block jen-fn %) cases)]
    (str "switch (" target-str ") " (jenerate-code-block jen-fn {:statements case-blocks}))))

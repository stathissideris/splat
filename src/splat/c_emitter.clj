(ns splat.c-emitter
  (:require [clojure.string :as str]
            [splat.ast :as ast])
  (:import [splat.ast
            CodeFile
            Statements
            PreDirective
            Function
            FunctionCall
            OpApplication
            Declaration
            Type
            StructDef
            Assignment
            ArrayAccess
            ArraySet
            FloatLiteral
            LongLiteral
            ForLoop
            WhileLoop
            IfThenElse
            LetBlock
            Return]))

(defn ->snake_case [s]
  (when s
    (let [guard "THISISASPLATARROW123456"] ;;TODO make less hacky
     (-> s
         (str/replace "->" guard)
         (str/replace "-" "_")
         (str/replace guard "->")))))

(defn check-str [s]
  (if-not (or (string? s) (symbol? s) (char? s))
    (throw (ex-info (str "Not a string or symbol: " (pr-str s)) {:v s}))
    s))
(defn check-coll [c]
  (if-not (or (seq? c) (list? c) (vector? c))
    (throw (ex-info (str "Not a list or vector: " (pr-str c)) {:v c}))
    c))

(defn double-quote [s] (str "\"" (check-str s) "\""))
(defn single-quote [s] (str "'" (check-str s) "'"))
(defn angle-quote [s] (str "<" (check-str s) ">"))
(defn paren [s] (str "(" (check-str s) ")"))
(defn statements [st]
  (let [st (remove nil? (check-coll st))]
    (str (str/join ";\n" st) ";\n")))
(defn block [expr] (str "{\n" (statements (check-coll expr)) "}\n"))
(defn commas [expr] (str/join ", " (remove nil? (check-coll expr))))
(defn spaces [expr] (str/join " " (remove nil? (check-coll expr))))
(defn lines [expr] (str/join "\n" (remove nil? (check-coll expr))))

(defmulti emit class)

(defmethod emit CodeFile [n]
  (lines (map emit (:expressions n))))

(defmethod emit Statements [s]
  (let [s (:statements s)]
    (if (= 1 (count s))
      (-> s first emit)
      (statements (map emit s)))))

(defmethod emit PreDirective [{:keys [directive params]}]
  (str "#"
       (subs (str directive) 1)
       " "
       (spaces
        (map (fn [p]
               (if (symbol? p)
                 (angle-quote p)
                 (double-quote p))) params))))

(defmethod emit Function [{:keys [declaration params body]}]
  (str (emit declaration) (paren (commas (map emit params)))  (block (map emit body))))

(defmethod emit FunctionCall [{:keys [name params]}]
  (str (->snake_case name) (paren (commas (map emit params)))))

(defmethod emit OpApplication [{:keys [op params]}]
  (if (= 1 (count params))
    (paren (str op " " (emit (first params))))
    (str (paren (str/join (str " " op " ") (map emit params))))))

(defmethod emit Type [{:keys [types const? restrict?
                              volatile? extern? pointer?
                              void? struct? array? array-size]}]
  (spaces
   (concat
    [(when extern? "extern")
     (when const? "const")
     (when void? "void")
     (when restrict? "restrict")
     (when volatile? "volatile")
     (when struct? "struct")]
    (map ->snake_case types)
    [(when pointer? "*")])))

(defmethod emit Declaration [{:keys [name type]}]
  (let [{:keys [array? array-size]} type]
    (spaces
     [(emit type)
      (str (->snake_case name)
           (when array?
             (if-not array-size "[]" (str "[" array-size "]"))))])))

(defmethod emit StructDef [{:keys [name members]}]
  (str "struct " (->snake_case name) (block (map emit members)) ";"))

(defmethod emit Assignment [{:keys [declaration value]}]
  (spaces [(emit declaration) "=" (emit value)]))

(defmethod emit ArrayAccess [{:keys [name index]}]
  (str (->snake_case name) "[" (emit index) "]"))

(defmethod emit ArraySet [{:keys [name index value]}]
  (apply str (->snake_case name) "[" (emit index) "] = " (emit value)))

(defmethod emit ForLoop [{:keys [init pred next body]}]
  (str "for" (paren (str/join "; " (map emit [init pred next])))
       (block (map emit body))))

(defmethod emit WhileLoop [{:keys [pred body]}]
  (str "while" (paren (emit pred))
       (block (map emit body))))

(defmethod emit IfThenElse [{:keys [test then else]}]
  (str "if "
       (paren (emit test))
       " "
       (block [(emit then)])
       (if else
         (str " else " (block [(emit else)])))))

(defmethod emit LetBlock [{:keys [bindings body]}]
  (block (map emit (concat bindings body))))

(defmethod emit Return [n]
  (str "return " (emit (:value n))))

(defmethod emit String [s]
  (-> s
      (str/replace "\n" "\\n")
      (str/replace "\"" "\\\"")
      (str/replace "\t" "\\t") ;;TODO much more here
      double-quote))

(defmethod emit Character [s] (single-quote s))

(defmethod emit clojure.lang.Symbol [s]
  (->snake_case s))

(defmethod emit clojure.lang.APersistentVector [v]
  (spaces ["{" (commas (map emit v)) "}"]))

(defmethod emit FloatLiteral [x]
  (str (:x x) "f"))

(defmethod emit LongLiteral [x]
  (str (:x x) "L"))

(defmethod emit nil [_]
  "NULL")

(defmethod emit :default [n] (str n))

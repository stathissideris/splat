(ns splat.c-emitter
  (:require [clojure.string :as str]
            [camel-snake-kebab.core :refer :all]
            [splat.ast :as ast])
  (:import [splat.ast
            CodeFile
            Statements
            PreDirective
            Function
            FunctionCall
            ArithmeticOp
            Declaration
            Assignment
            ArrayAccess
            ArraySet
            FloatLiteral
            LongLiteral
            ForLoop
            WhileLoop
            LetBlock
            Return]))

(defn double-quote [s] (str "\"" s "\""))
(defn single-quote [s] (str "'" s "'"))
(defn angle-quote [s] (str "<" s ">"))
(defn paren [s] (str "(" s ")"))
(defn statements [s]
  (let [s (remove nil? s)]
    (str (str/join ";\n" s) ";\n")))
(defn block [expr] (str "{\n" (statements expr) "}\n"))
(defn commas [expr] (str/join ", " (remove nil? expr)))
(defn spaces [expr] (str/join " " (remove nil? expr)))
(defn lines [expr] (str/join "\n" (remove nil? expr)))

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
  (str name (paren (commas (map emit params)))))

(defmethod emit ArithmeticOp [{:keys [op params]}]
  (str (paren (str/join (str " " op " ") (map emit params)))))

(defmethod emit Declaration [{:keys [name types const? restrict? volatile? extern? pointer? void? array? array-size]}]
  (spaces
   (concat
    [(when extern? "extern")
     (when const? "const")
     (when void? "void")
     (when restrict? "restrict")
     (when volatile? "volatile")]
    types
    [(str (when pointer? "*")
          (->snake_case name)
          (when array?
            (if-not array-size "[]" (str "[" array-size "]"))))])))

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

(defmethod emit LetBlock [{:keys [bindings body]}]
  (block (map emit (concat bindings body))))

(defmethod emit Return [n]
  (str "return " (emit (:value n))))

(defmethod emit String [s]
  (-> s
      (str/replace "\n" "\\n")
      (str/replace "\"" "\\\"") ;;TODO much more here
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

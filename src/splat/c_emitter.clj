(ns splat.c-emitter
  (:require [clojure.string :as str]
            [camel-snake-kebab.core :refer :all])
  (:import [splat.ast
            CodeFile
            PreDirective
            Function
            FunctionCall
            ArithmeticOp
            Declaration
            Assignment
            ArrayAccess
            Return]))

(defn double-quote [s] (str "\"" s "\""))
(defn single-quote [s] (str "'" s "'"))
(defn angle-quote [s] (str "<" s ">"))
(defn block [expr] (str "{\n" (str/join ";\n" (remove nil? expr)) ";\n}\n"))
(defn commas [expr] (str/join ", " (remove nil? expr)))
(defn spaces [expr] (str/join " " (remove nil? expr)))
(defn lines [expr] (str/join "\n" (remove nil? expr)))

(defmulti emit class)

(defmethod emit CodeFile [n]
  (lines (map emit (:expressions n))))

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
  (str (emit declaration) "(" (commas (map emit params)) ")" (block (map emit body))))

(defmethod emit FunctionCall [{:keys [name params]}]
  (str name "(" (commas (map emit params)) ")"))

(defmethod emit ArithmeticOp [{:keys [op params]}]
  (str "(" (str/join (str " " op " ") (map emit params)) ")"))

(defmethod emit Declaration [{:keys [name types const? restrict? volatile? extern? pointer? void? array?]}]
  (spaces
   (concat
    [(when extern? "extern")
     (when const? "const")
     (when void? "void")
     (when restrict? "restrict")
     (when volatile? "volatile")]
    types
    [(str (when pointer? "*") (->snake_case name) (when array? "[]"))])))

(defmethod emit Assignment [{:keys [declaration value]}]
  (spaces [(emit declaration) "=" (emit value)]))

(defmethod emit ArrayAccess [{:keys [name index]}]
  (str (->snake_case name) "[" (emit index) "]"))

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

(defmethod emit nil [_]
  "NULL")

(defmethod emit :default [n] (str n))

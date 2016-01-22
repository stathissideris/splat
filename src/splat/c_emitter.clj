(ns splat.c-emitter
  (:require [clojure.string :as str])
  (:import [splat.ast
            CodeFile
            PreDirective
            Function
            FunctionCall
            ArithmeticOp
            Declaration
            Return]))

(defn double-quote [s] (str "\"" s "\""))
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

(defmethod emit Function [{:keys [name return-type params body]}]
  (str return-type " " name "(" (commas (map emit params)) ")" (block (map emit body))))

(defmethod emit FunctionCall [{:keys [name params]}]
  (str name "(" (commas (map (fn [p] (if (string? p) (emit p) p)) params)) ")"))

(defmethod emit ArithmeticOp [{:keys [op params]}]
  (str "(" (str/join (str " " op " ") (map emit params)) ")"))

(defmethod emit String [s]
  (-> s
      (str/replace "\n" "\\n") ;;TODO much more here
      double-quote))

(defmethod emit Declaration [{:keys [name types const? restrict? volatile? extern? pointer? void?]}]
  (spaces
   (concat
    [(when extern? "extern")
     (when const? "const")
     (when restrict? "restrict")
     (when volatile? "volatile")]
    types
    [(str (when pointer? "*") name)])))

(defmethod emit Return [n]
  (str "return " (emit (:value n))))

(defmethod emit :default [n] (str n))

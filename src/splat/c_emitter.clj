(ns splat.c-emitter
  (:require [clojure.string :as str])
  (:import [splat.ast
            CodeFile
            PreDirective
            Function
            Return
            FunctionCall]))

(defn double-quote [s] (str "\"" s "\""))
(defn angle-quote [s] (str "<" s ">"))
(defn block [expr] (str "{\n" (str/join ";\n" expr) ";\n}\n"))
(defn commas [expr] (str/join ", " expr))
(defn spaces [expr] (str/join " " expr))
(defn lines [expr] (str/join "\n" expr))

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
  (str return-type " " name "()" (block (map emit body))))

(defmethod emit FunctionCall [{:keys [name params]}]
  (str name "(" (commas (map (fn [p] (if (string? p) (emit p) p)) params)) ")"))

(defmethod emit String [s]
  (-> s
      (str/replace "\n" "\\n") ;;TODO much more here
      double-quote))

(defmethod emit Return [n]
  (str "return " (:value n)))

(defmethod emit :default [n] (str n))

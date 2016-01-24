(ns splat.macros
  (:require [clojure.zip :as zip]
            [splat.util :as util]))

(def core-macros
  {'cond
   (fn [& clauses]
     (when clauses
       (list 'if (first clauses)
             (if (next clauses)
               (second clauses)
               (throw (IllegalArgumentException.
                       "cond requires an even number of forms")))
             (cons 'cond (next (next clauses))))))})

(defn extract-macros [source]
  (loop [zipper (util/generic-zipper source)
         macros {}]
    (let [node (zip/node zipper)]
      (cond (zip/end? zipper)
            macros

            (and (list? node) (= 'defmacro (first node)))
            (let [[_ name & body] node]
              (recur
               (zip/next zipper)
               (assoc macros name (eval (cons 'fn body)))))

            :else
            (recur (zip/next zipper) macros)))))

(defn remove-macros [source]
  (util/walk-zipper
   (util/generic-zipper source)
   (fn [z]
     (let [node (zip/node z)]
       (if (and (list? node) (= 'defmacro (first node)))
         (zip/remove z)
         z)))))

(defn apply-macros [macros source]
  (util/transform-zipper
   (util/generic-zipper source)
   (fn [z]
     (let [node (zip/node z)]
       (if-not (seq? node)
         node
         (let [op    (first node)
               macro (macros op)]
           (if-not macro
             node
             (apply macro (rest node)))))))))

(defn converge-macros [macros source]
  (util/converge
   (fn [s] (apply-macros macros s))
   source))

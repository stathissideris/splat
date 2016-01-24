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

(defn apply-macros [macros source]
  (let [z (util/generic-zipper source)]
    (util/transform-zipper
     z
     (fn [z]
       (let [node (zip/node z)]
         (if-not (seq? node)
           node
           (let [op    (first node)
                 macro (macros op)]
             (if-not macro
               node
               (apply macro (rest node))))))))))

(defn converge-macros [macros source]
  (util/converge
   (fn [s] (apply-macros macros s))
   source))

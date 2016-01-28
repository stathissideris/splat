(ns splat.macros
  (:require [clojure.zip :as zip]
            [splat.util :as util]))

(def core-macros
  {'when (fn [test & body] (list 'if test (cons 'do body)))
   'cond
   (fn [& clauses]
     (when clauses
       (list 'if (first clauses)
             (if (next clauses)
               (second clauses)
               (throw (IllegalArgumentException.
                       "cond requires an even number of forms")))
             (cons 'cond (next (next clauses))))))
   '->
   (fn [x & forms]
     (loop [x x, forms forms]
       (if forms
         (let [form (first forms)
               threaded (if (seq? form)
                          (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                          (list form x))]
           (recur threaded (next forms)))
         x)))
   '->>
   (fn ->> [x & forms]
     (loop [x x, forms forms]
       (if forms
         (let [form (first forms)
               threaded (if (seq? form)
                          (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
                          (list form x))]
           (recur threaded (next forms)))
         x)))})

(defn clean-up-macro
  "The reader will qualify bare symbols when reading the macro, this
  function will clean the namespace prefixes up."
  [source]
  (util/walk-zipper
   (util/generic-zipper source)
   (fn [z]
     (let [node (zip/node z)]
       (if (= 'quote node)
         (let [namespaced-sym (-> z zip/right zip/node)]
           (-> z zip/right (zip/replace (-> namespaced-sym name symbol))))
         z)))))

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
               (assoc macros name (eval (clean-up-macro (cons 'fn body))))))

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

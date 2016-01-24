(ns splat.util
  (:require [clojure.java.io :as io]
            ;;[clojure.tools.reader :as reader]
            ;;[clojure.tools.reader.reader-types :as reader-types]
            [clojure.edn :as edn]
            [clojure.zip :as zip]
            [splat.ast :as ast]))

(defn read-edn [filename]
  (edn/read-string
   {:readers
    {'float ast/->FloatLiteral
     'long  ast/->LongLiteral}}
   (str "(" (slurp filename) ")")))

(defn converge
  "Like (iterate) but stops when the result of iterating stops
  changing or at 1000 iterations."
  [fun coll]
  (->> (iterate fun coll)
       (take 1000)
       (partition 2 1)
       (take-while (fn [[a b]] (not= a b)))
       last
       last))

(defn generic-zipper
  "Walks vectors, lists, maps, and maps' keys and values
  individually. Take care not to replace a keypair with a single
  value (will throw an exception)."
  [x]
  (zip/zipper
   (fn [x]
     (and (not (record? x))
          (or (sequential? x) (map? x))))
   seq
   (fn [node children]
     (cond (vector? node) (vec children)
           (list? node) (apply list children)
           (seq? node) (seq children)
           (map? node) (into {} children)))
   x))

(defn- zipper-last [zipper]
  (loop [zipper zipper]
    (if (zip/end? (zip/next zipper)) zipper (recur (zip/next zipper)))))

(defn transform-zipper-backwards
  ([z replace-fn]
   (loop [z (zipper-last z)]
     (let [p (zip/prev (zip/replace z (replace-fn z)))]
       (if (nil? p) ;;we've hit top, no previous node
         (zip/node z)
         (recur p))))))

(defn transform-zipper [zipper replace-fn]
  (loop [zipper zipper]
    (if (zip/end? zipper)
      (zip/root zipper)
      (recur (zip/next (zip/replace
                        zipper
                        (replace-fn zipper)))))))

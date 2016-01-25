(ns splat.util
  (:require [clojure.java.io :as io]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]
            [clojure.edn :as edn]
            [clojure.zip :as zip]
            [splat.ast :as ast]))

(defn read-edn
  [filename]
  (with-open [rdr (io/reader (io/file filename))]
    (binding [reader/*data-readers* {'float ast/->FloatLiteral
                                     'long ast/->LongLiteral}]
     (let [rdr (reader-types/indexing-push-back-reader (java.io.PushbackReader. rdr))]
       (try
         (loop [out []]
           (let [data (reader/read {:eof ::eof} rdr)]
             (if (= ::eof data)
               out
               (recur (conj out data)))))
         (catch Exception e
           (when-let [{:keys [file line column] :as data} (ex-data e)]
             (throw (ex-info (format "Error in '%s' line %d col %s: %s"
                                     filename line column (.getMessage e))
                             (assoc data :reason ::read-error)
                             e)))))))))

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

(defn- wipe-map [m]
  (reduce (fn [m k] (assoc m k nil)) m (keys m)))

(defn ast-zipper
  "This will walk records."
  [x]
  (zip/zipper
   (fn [x]
     (or (sequential? x) (map? x)))
   seq
   (fn [node children]
     (cond (vector? node) (vec children)
           (list? node) (apply list children)
           (seq? node) (seq children)
           (map? node) (into (wipe-map node) children)))
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

(defn walk-zipper [zipper fun]
  (loop [zipper zipper]
    (if (zip/end? zipper)
      (zip/root zipper)
      (recur (zip/next (fun zipper))))))

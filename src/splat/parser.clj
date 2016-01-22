(ns splat.parser
  (:require [clojure.tools.reader.edn :as edn]
            [clojure.zip :as zip]
            [clojure.string :as str]
            [splat.ast :as ast]
            [splat.util :as util]))

(defn generic-zipper
  "Walks vectors, lists, maps, and maps' keys and values
  individually. Take care not to replace a keypair with a single
  value (will throw an exception)."
  [x]
  (zip/zipper
   (some-fn sequential? map?)
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

(defn- transform-zipper-backwards
  ([z replace-fn]
   (loop [z (zipper-last z)]
     (let [p (zip/prev (zip/replace z (replace-fn z)))]
       (if (nil? p) ;;we've hit top, no previous node
         (zip/node z)
         (recur p))))))

(defn- transform-zipper [zipper replace-fn]
  (loop [zipper zipper]
    (if (zip/end? zipper)
      (zip/root zipper)
      (recur (zip/next (zip/replace
                        zipper
                        (replace-fn zipper)))))))

(defn- pre-directive? [node]
  (and (list? node)
    (let [x (first node)]
      (and (symbol? x) (str/starts-with? (name x) "!")))))

(defn- function-def? [node]
  (and (list? node) (= 'defn (first node))))

(defn- function-call? [node]
  (and (list? node) (symbol? (first node))))

(defn- arithmetic-op? [node]
  (and (list? node)
       (#{'+ '- '/ '*} (first node))))

(defn assign? [node]
  (and (list? node) (= '=> (first node))))

(defn- var-name [s]
  (-> s str (str/replace "*" "") symbol))

(defn- pointer-name? [s]
  (-> s str (str/starts-with? "*")))

(defn- parse-var-declaration [tokens]
  (let [d (ast/map->Declaration {:name     (var-name (last tokens))
                                 :pointer? (pointer-name? (last tokens))
                                 :types    []})]
    (loop [tokens (butlast tokens)
           d      d]
      (if-not (seq? tokens)
        d
        (recur
         (next tokens)
         (let [t (first tokens)]
           (condp = t
             'const    (assoc d :const? true)
             'restrict (assoc d :restrict? true)
             'volatile (assoc d :volatile? true)
             'extern   (assoc d :extern? true)
             'void     (assoc d :void? true)
             (update d :types conj t))))))))

(defn- parse-function-params [params]
  (->> params
       (partition-by (fn [x] (= x '.)))
       (take-nth 2)
       (map parse-var-declaration)))

(declare parse)

(defn- parse-position [z]
  (let [node (zip/node z)]
    (cond (pre-directive? node)
          (ast/pre-directive (first node) (rest node))

          (function-def? node)
          (let [[_ return-type name params & body] node]
            (ast/function name return-type (parse-function-params params) body))

          (arithmetic-op? node)
          (ast/arithmetic-op (first node) (rest node))

          (assign? node)
          (let [node (rest node)]
            (ast/assignment (parse-var-declaration (butlast node))
                            (parse (last node))))
          
          (function-call? node)
          (if (= 'return (first node))
            (ast/return (second node))
            (ast/function-call (first node) (rest node)))

          :else node)))

(defn parse [source]
  (transform-zipper-backwards (generic-zipper source) parse-position))

(defn parse-file [filename]
  (ast/code-file
   (parse (util/read-edn filename))))

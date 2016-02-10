(ns splat.compiler
  (:require [clojure.zip :as zip]
            [splat.util :as util]
            [splat.ast :as ast]
            [splat.parser :as parser])
  (:import [splat.ast Lambda PreDirective FloatLiteral LongLiteral]))

(def lambda-count (atom 0))

(defn- make-fn-name []
  (symbol (str "SPLAT-LAMBDA-" (swap! lambda-count inc))))

(defn swap-lambdas [source]
  (loop [zipper (util/ast-zipper source)
         lambdas {}]
    (let [node (zip/node zipper)]
      (cond
        (zip/end? zipper)
        {:source (zip/root zipper)
         :lambdas lambdas}

        (ast/lambda? node)
        (let [fn-name (make-fn-name)]
          (recur
           (zip/next (zip/replace zipper fn-name))
           (assoc lambdas fn-name node)))

        :else (recur (zip/next zipper) lambdas)))))

(defn- lambda->function [name lambda]
  (ast/map->Function
   {:declaration (ast/->Declaration name (:return lambda))
    :params      (:params lambda)
    :body        (:body lambda)}))

(defn inject-lifted-lambdas [source lambdas]
  (loop [zipper (util/ast-zipper source)]
    (let [node (zip/node zipper)]
      (cond
        (zip/end? zipper)
        (zip/root zipper)

        (ast/pre-directive? node)
        (if (-> zipper zip/right zip/node ast/pre-directive?)
          (recur (zip/next zipper))
          (recur
           (zip/next
            (zip/insert-right
             zipper
             (ast/->Statements
              (map (fn [[n l]] (lambda->function n l)) lambdas))))))

        :else (recur (zip/next zipper))))))

(defn- types [& types]
  (ast/map->Type {:types types}))

(declare infer-type)

(defn- scan-array-type [v]
  (let [types (set (remove nil? (map infer-type v)))]
    (if (empty? types)
      (throw (ex-info "Cannot infer type of array" {:array v}))
      (first types)))) ;;best effort, if more than one let C compiler catch it

(defn- infer-array-type [v]
  (if (every? (complement vector?) v)
    ;;1d vector
    (assoc (scan-array-type v) :arrays [:empty])
    ;;more dimensions
    (let [element-type (first (flatten v))]
      (loop [v           v
             array-sizes [(count v)]]
        (if-not (vector? (first v))
          (assoc (scan-array-type v) :arrays array-sizes)
          (recur (first v) (conj array-sizes (apply max (map count v)))))))))

(defn- infer-type [v]
  (condp = (type v)
    FloatLiteral (types "float")
    LongLiteral  (types "long")
    Double       (types "double")
    String       (parser/parse-type 'char:ptr)
    (cond (integer? v) (parser/parse-type 'int)
          (vector? v)  (infer-array-type v)
          :else        nil)))

(defn- value->type [v]
  (or (infer-type v)
      (throw (ex-info "Cannot infer type of expression"
                      {:expression v
                       :type       (type v)}))))

(defn infer-types [source]
  (util/walk-zipper
   (util/ast-zipper source)
   (fn [zipper]
     (let [node (zip/node zipper)]
       (if (and (ast/assignment? node) (symbol? (:left node)))
         (zip/replace
          zipper
          (assoc node :left (ast/->Declaration
                             (:left node)
                             (value->type (:value node)))))
         zipper)))))

(defn compile-ast [source]
  (let [{:keys [source lambdas]} (swap-lambdas source)]
    (-> (if-not (empty? lambdas)
          (inject-lifted-lambdas source lambdas)
          source)
        infer-types)))

(ns splat.compiler
  (:require [clojure.zip :as zip]
            [splat.util :as util]
            [splat.ast :as ast])
  (:import [splat.ast Lambda PreDirective]))

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

(defn compile-ast [source]
  (let [{:keys [source lambdas]} (swap-lambdas source)]
    (if-not (empty? lambdas)
      (inject-lifted-lambdas source lambdas)
      source)))

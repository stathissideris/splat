(ns splat.ast
  (:require [clojure.string :as str]
            [camel-snake-kebab.core :refer :all]))

(defmacro ast-node [name params]
  `(do
     (defrecord ~name ~params)
     (defn ~(->kebab-case name) ~params
       (~(symbol (str name \.)) ~@params))))

(ast-node CodeFile [expressions])
(ast-node PreDirective [directive params])
(ast-node Function [name return-type params body])
(ast-node FunctionCall [name params])
(ast-node ArithmeticOp [op params])
(ast-node Declaration [name types const? restrict? volatile? extern? pointer? void?])
(ast-node Return [value])

(ns splat.ast
  (:require [clojure.string :as str]
            [camel-snake-kebab.core :refer :all]))

(defmacro ast-node [name params]
  `(do
     (defrecord ~name ~params)
     (defn ~(->kebab-case name) ~params
       (~(symbol (str name \.)) ~@params))))

(ast-node CodeFile [expressions])
(ast-node Statements [statements])
(ast-node PreDirective [directive params])
(ast-node Function [declaration params body])
(ast-node FunctionCall [name params])
(ast-node ArithmeticOp [op params])
(ast-node Declaration [name types const? restrict? volatile? extern? pointer? void? array? array-size])
(ast-node Assignment [declaration value])
(ast-node ArrayAccess [name index])
(ast-node ArraySet [name index value])
(ast-node ForLoop [init pred next body])
(ast-node Return [value])

(ast-node FloatLiteral [x])
(ast-node LongLiteral [x])

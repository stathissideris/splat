(ns splat.ast
  (:require [clojure.string :as str]
            [camel-snake-kebab.core :refer :all]))

(defmacro ast-node [name params]
  `(do
     (defrecord ~name ~params)
     (defn ~(symbol (str (->kebab-case name) "?")) [~'x]
       (instance? ~name ~'x))))

(ast-node CodeFile [expressions])
(ast-node NoOp [])
(ast-node Statements [statements])
(ast-node PreDirective [directive params])
(ast-node Function [declaration params body])
(ast-node Lambda [return params body])
(ast-node FunctionCall [name params])
(ast-node OpApplication [op params])
(ast-node Declaration [name type])
(ast-node Pointer [expression])
(ast-node FunctionPointer [return params])
(ast-node DefType [declaration])
(ast-node Type [types const? restrict?
                volatile? extern? pointer-level
                void? struct? arrays])
(ast-node Cast [type expr])
(ast-node StructDef [name members])
(ast-node Assignment [left value])
(ast-node MemberAccess [member-name expression])
(ast-node MemberPointerAccess [member-name expression])
(ast-node ArrayAccess [name index])
(ast-node ArraySet [name index value])
(ast-node ForLoop [init pred next body])
(ast-node IfThenElse [test then else])
(ast-node WhileLoop [pred body])
(ast-node LetBlock [bindings body])
(ast-node Return [value])

(ast-node FloatLiteral [x])
(ast-node LongLiteral [x])

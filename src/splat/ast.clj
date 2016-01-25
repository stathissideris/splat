(ns splat.ast
  (:require [clojure.string :as str]))

(defrecord CodeFile [expressions])
(defrecord NoOp [])
(defrecord Statements [statements])
(defrecord PreDirective [directive params])
(defrecord Function [declaration params body])
(defrecord FunctionCall [name params])
(defrecord OpApplication [op params])
(defrecord Declaration [name type])
(defrecord DefType [declaration])
(defrecord Type [types const? restrict?
                 volatile? extern? pointer-level
                 void? struct? arrays])
(defrecord StructDef [name members])
(defrecord Assignment [declaration value])
(defrecord ArrayAccess [name index])
(defrecord ArraySet [name index value])
(defrecord ForLoop [init pred next body])
(defrecord IfThenElse [test then else])
(defrecord WhileLoop [pred body])
(defrecord LetBlock [bindings body])
(defrecord Return [value])

(defrecord FloatLiteral [x])
(defrecord LongLiteral [x])

(ns splat.util
  (:require [clojure.java.io :as io]
            ;;[clojure.tools.reader :as reader]
            ;;[clojure.tools.reader.reader-types :as reader-types]
            [clojure.edn :as edn]
            [splat.ast :as ast]))

(defn read-edn [filename]
  (edn/read-string
   {:readers
    {'float ast/->FloatLiteral
     'long  ast/->LongLiteral}}
   (str "(" (slurp filename) ")")))

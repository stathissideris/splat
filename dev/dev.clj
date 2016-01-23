(ns dev
  (:require [splat.tools :as tools]
            [splat.parser :as parser]
            [splat.c-emitter :as emitter]
            [splat.ast :as ast]
            [clojure.tools.namespace.repl :refer [clear refresh-all]]
            [clojure.pprint :refer [pprint]]))

(defn refresh []
  (clojure.tools.namespace.repl/refresh))

(def transpile-file tools/transpile-file)
(def compile-file tools/compile-file)
(def parse-file parser/parse-file)
(def parse parser/parse)
(def emit emitter/emit)

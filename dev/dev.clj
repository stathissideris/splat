(ns dev
  (:require [splat.tools :as tools]
            [splat.parser :as parser]
            [splat.c-emitter :as emitter]
            [clojure.tools.namespace.repl :refer [clear refresh-all]]))

(defn refresh []
  (clojure.tools.namespace.repl/refresh))

(def transpile-file tools/transpile-file)
(def compile-file tools/compile-file)
(def parse-file parser/parse-file)
(def emit emitter/emit)

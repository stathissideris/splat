(ns dev
  (:require [splat.tools :as tools]
            [splat.parser :as parser]
            [splat.c-emitter :as emitter]
            [splat.ast :as ast]
            [clojure.tools.namespace.repl :refer [clear refresh-all]]
            [clojure.pprint :refer [pprint]]
            [me.raynes.fs :as fs]))

(defn refresh []
  (clojure.tools.namespace.repl/refresh))

(def transpile-file tools/transpile-file)
(def compile-file tools/compile-file)
(def parse-file parser/parse-file)
(def parse parser/parse)
(def emit emitter/emit)

(defn compile-all []
  (let [sources (fs/list-dir "resources/examples/")]
    (doseq [f sources]
      (print "Compiling" (fs/name f))
      (println " -" (compile-file f "resources/target/")))))

(defn compile-example [s]
  (compile-file (str "resources/examples/" s) "resources/target/"))

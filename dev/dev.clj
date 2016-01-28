(ns dev
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.tools.namespace.repl :refer [clear refresh-all]]
            [me.raynes.fs :as fs]
            [splat.ast :as ast]
            [splat.c-emitter :as emitter]
            [splat.parser :as parser]
            [splat.tools :as tools]
            [splat.util :as util]))

(def ex-folder "resources/examples/")

(defn refresh []
  (clojure.tools.namespace.repl/refresh))

(def read-file #'util/read-edn)
(def transpile-file #'tools/transpile-file)
(def compile-file #'tools/compile-file)
(def parse-file #'parser/parse-file)
(def parse-source #'parser/parse-source)
(def macroexpand-source #'tools/macroexpand)
(def emit #'emitter/emit)

(defn format-c-file [filename]
  (-> (Runtime/getRuntime)
      (.exec (into-array String ["/usr/local/bin/uncrustify"
                                 "-c" "/Users/sideris/devel/splat/resources/uncrustify-config"
                                 "--no-backup"
                                 filename]))
      .waitFor))

(defn compile-all []
  (let [sources (filter #(str/ends-with? (fs/base-name %) ".splat")
                        (fs/list-dir "resources/examples/"))]
    (doseq [f sources]
      (print "Compiling" (fs/name f))
      (println " -" (compile-file f "resources/target/"))
      (format-c-file (str "resources/target/" (fs/name f) ".c")))))

(defn compile-example [s]
  (compile-file (str "resources/examples/" s) "resources/target/"))

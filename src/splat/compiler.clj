(ns splat.compiler
  (:require [splat.parser :as parser]
            [splat.c-emitter :as emitter]
            [me.raynes.fs :as fs])) 

(defn transpile-file [input-file output-file]
  (->> input-file
       parser/parse-file
       emitter/emit
       (spit output-file)))

(defn clang [in out]
  (-> (Runtime/getRuntime)
      (.exec (into-array String ["clang" (str in) "-o" (str out)]))))

(defn compile-file [input-file]
  (let [code-file (fs/file (fs/parent "resources/hello2.splat")
                           (str (fs/name "resources/hello2.splat") ".c"))
        compiled-file (fs/file (fs/parent "resources/hello2.splat")
                               (fs/name "resources/hello2.splat"))]
    (transpile-file input-file code-file)
    (clang code-file compiled-file)))

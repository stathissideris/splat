(ns splat.tools
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
      (.exec (into-array String ["clang" (str in) "-o" (str out)]))
      .waitFor))

(defn compile-file [input-file]
  (let [code-file (fs/file (fs/parent input-file)
                           (str (fs/name input-file) ".c"))
        compiled-file (fs/file (fs/parent input-file)
                               (fs/name input-file))]
    (transpile-file input-file code-file)
    (clang code-file compiled-file)))

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
      (.exec (into-array String ["clang" (str in) "-g" "-o" (str out)]))
      .waitFor))

(defn compile-file
  ([input-file]
   (compile-file input-file nil))
  ([input-file out-dir]
   (let [out-dir       (or out-dir (fs/parent input-file))
         c-file        (fs/file out-dir (str (fs/name input-file) ".c"))
         compiled-file (fs/file out-dir (fs/name input-file))]
     (transpile-file input-file c-file)
     (clang c-file compiled-file))))

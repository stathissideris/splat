(ns splat.tools
  (:refer-clojure :exclude [macroexpand])
  (:require [splat.macros :as macros]
            [splat.parser :as parser]
            [splat.compiler :as compiler]
            [splat.util :as util]
            [splat.c-emitter :as emitter]
            [me.raynes.fs :as fs])) 

(defn macroexpand [source]
  (let [macros (macros/extract-macros source)
        source (macros/remove-macros source)]
    (macros/apply-macros (merge macros/core-macros macros) source)))

(defn transpile-file [input-file output-file]
  (let [source (util/read-edn input-file)]
    ;;TODO add warning for shadowing macros
    (def aa (->> source
                 macroexpand
                 parser/parse-source))
    (def ss (->> source
                 macroexpand
                 parser/parse-source
                 compiler/compile-ast))
    (->> source
         macroexpand
         parser/parse-source
         compiler/compile-ast
         emitter/emit
         (spit output-file))))

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

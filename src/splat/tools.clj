(ns splat.tools
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.string :as str]
            [me.raynes.fs :as fs]
            [nuka.exec :refer [run-command >print exit-code]]
            [nuka.script.java :as script]
            [nuka.script :refer [script call pipe chain-and chain-or block] :as s]
            [splat.c-emitter :as emitter]
            [splat.compiler :as compiler]
            [splat.macros :as macros]
            [splat.parser :as parser]
            [splat.util :as util]))

(defn macroexpand [source]
  (let [macros (macros/extract-macros source)
        source (macros/remove-macros source)]
    (macros/apply-macros (merge macros/core-macros macros) source)))

(defn transpile-file [input-file output-file]
  (let [source (util/read-edn input-file)]
    ;;TODO add warning for shadowing macros
    (->> source
         macroexpand
         parser/parse-source
         compiler/compile-ast
         emitter/emit
         (spit output-file))))

(defn clang [in out]
  (let [proc (run-command (call :clang "-Wno-parentheses-equality" {:g true, :o (str out)} (str in)))]
    (>print proc) ;;establishes a consumer, non-blocking
    (let [e (exit-code proc)]
      (println "|")
      (println "\\-> EXIT-CODE:" e)
      e)))

(defn compile-file
  ([input-file]
   (compile-file input-file nil))
  ([input-file out-dir]
   (let [out-dir       (or out-dir (fs/parent input-file))
         c-file        (fs/file out-dir (str (fs/name input-file) ".c"))
         compiled-file (fs/file out-dir (fs/name input-file))]
     (transpile-file input-file c-file)
     (clang c-file compiled-file))))

(defn run-file
  [input-file & params]
  (let [compile-exit-code (compile-file input-file "/tmp")]
    (if-not (zero? compile-exit-code)
     (println "Compilation failed with code" compile-exit-code)
     (do
       (println "\nRunning...")
       (let [proc (run-command (call (str (fs/file "/tmp" (fs/name input-file))) params))]
         (>print proc)
         (exit-code proc))))))


(defn- write-lines [file-path lines]
  (with-open [wtr (clojure.java.io/writer file-path)]
    (doseq [line lines]
      (.write wtr line)
      (.write wtr "\n"))))

(defn eval-source
  "Not a real eval (it only returns the exit code)"
  [source & params]
  (let [source-file (fs/temp-file "splat-eval")]
    (->>
     `[(~'!include ~'stdio.h)
       (~'defn ~'main:int [~'argc:int ~'argv:arr:char:ptr]
         ~source
         (~'return 0))]
     (map pr-str)
     (write-lines source-file))
    (apply run-file source-file params))) ;;TODO remove the files

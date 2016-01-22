(ns user
  (:require [clojure.tools.namespace.repl :refer [clear refresh-all]]))

(defn refresh []
  (clojure.tools.namespace.repl/refresh))

(defn dev []
  (require 'dev)
  (in-ns 'dev))

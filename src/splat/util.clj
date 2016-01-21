(ns splat.util
  (:require [clojure.java.io :as io]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]))

(defn read-edn
  [filename]
  (with-open [rdr (io/reader (io/file filename))]
    (let [rdr (reader-types/indexing-push-back-reader (java.io.PushbackReader. rdr))]
      (try
        (loop [out []]
          (let [data (reader/read {:eof ::eof} rdr)]
            (if (= ::eof data)
              out
              (recur (conj out data)))))
        (catch Exception e
          (when-let [{:keys [file line column] :as data} (ex-data e)]
            (throw (ex-info (format "Error in '%s' line %d col %s: %s"
                                    filename line column (.getMessage e))
                            (assoc data :reason ::read-error)
                            e))))))))

(ns splat.parser
  (:require [clojure.tools.reader.edn :as edn]
            [clojure.zip :as zip]
            [clojure.string :as str]
            [splat.ast :as ast]
            [splat.util :as util]))

(def bit-operator->sym
  {'bit-or  (symbol "|")
   'bit-and (symbol "&")
   'bit-xor (symbol "^")
   'bit-not (symbol "~")
   '>>      (symbol ">>")
   '<<      (symbol "<<")})

(defn bit-operator? [n]
  (and (list? n) (get (set (keys bit-operator->sym)) (first n))))

(defn generic-zipper
  "Walks vectors, lists, maps, and maps' keys and values
  individually. Take care not to replace a keypair with a single
  value (will throw an exception)."
  [x]
  (zip/zipper
   (fn [x]
     (and (not (record? x))
          (or (sequential? x) (map? x))))
   seq
   (fn [node children]
     (cond (vector? node) (vec children)
           (list? node) (apply list children)
           (seq? node) (seq children)
           (map? node) (into {} children)))
   x))

(defn- zipper-last [zipper]
  (loop [zipper zipper]
    (if (zip/end? (zip/next zipper)) zipper (recur (zip/next zipper)))))

(defn- transform-zipper-backwards
  ([z replace-fn]
   (loop [z (zipper-last z)]
     (let [p (zip/prev (zip/replace z (replace-fn z)))]
       (if (nil? p) ;;we've hit top, no previous node
         (zip/node z)
         (recur p))))))

(defn- transform-zipper [zipper replace-fn]
  (loop [zipper zipper]
    (if (zip/end? zipper)
      (zip/root zipper)
      (recur (zip/next (zip/replace
                        zipper
                        (replace-fn zipper)))))))

(defn- pre-directive? [node]
  (and (list? node)
       (let [x (first node)]
         (and (symbol? x) (str/starts-with? (name x) "!")))))

(defn- first= [coll item]
  (and (list? coll) (= item (first coll))))

(defn- function-call? [node]
  (and (list? node) (symbol? (first node))))

(defn- operator? [node]
  (and (list? node)
       (#{'+ '- '/ '* '< '> '<= '>= '== '!=}
        (first node))))

(defn- var-name [s]
  (-> s str (str/replace "*" "") symbol))

(defn- pointer-name? [s]
  (-> s str (str/starts-with? "*")))

(defn- parse-int [s]
  (try (Integer/parseInt s)
       (catch Exception _ nil)))

(defn- parse-type-tokens [tokens]
  (loop [tokens tokens
         d      {}]
    (if-not (seq tokens)
      d
      (recur
       (next tokens)
       (let [t (first tokens)]
         (if-let [x (parse-int t)]
           (assoc d :array-size x)
           (condp = t
             "const"    (assoc d :const? true)
             "restrict" (assoc d :restrict? true)
             "volatile" (assoc d :volatile? true)
             "extern"   (assoc d :extern? true)
             "void"     (assoc d :void? true)
             "struct"   (assoc d :struct? true)
             "array"    (assoc d :array? true)
             "*"        (assoc d :pointer? true)
             (update d :types conj t))))))))

(defn- parse-var-declaration [decl]
  (let [tokens   (-> decl str (str/split #":"))
        var-name (var-name (first tokens))
        d        (ast/map->Declaration {:name var-name})
        t        (merge (ast/map->Type {}) (parse-type-tokens (rest tokens)))
        t        (if (pointer-name? (first tokens))
                   (assoc t :pointer? true)
                   t)]
    (assoc d :type t)))

(defn- parse-type [decl]
  (let [tokens (-> decl str (str/split #":"))]
    (merge (ast/map->Type {}) (parse-type-tokens tokens))))

(defn- parse-function-params [params]
  (map parse-var-declaration params))

(declare parse)

(defn- parse-assignment [node]
  (let [pairs (partition 2 (rest node))]
    (ast/->Statements
     (for [[decl value] pairs]
       (ast/->Assignment (parse-var-declaration decl)
                         (parse value))))))

(defn- parse-position [z]
  (let [node (zip/node z)]
    (cond (operator? node)
          (ast/->ArithmeticOp (first node) (rest node))

          (pre-directive? node)
          (ast/->PreDirective (first node) (rest node))

          (first= node 'defn)
          (let [[_ decl params & body] node]
            (ast/->Function (parse-var-declaration decl) (parse-function-params params) body))

          (first= node 'set!)
          (parse-assignment node)

          (first= node 'aget)
          (let [[_ name index] node]
            (ast/->ArrayAccess name index))

          (first= node 'aset!)
          (let [[_ name index value] node]
            (ast/->ArraySet name index value))

          (first= node 'for)
          (let [[_ init pred index & body] node]
            (ast/->ForLoop init pred index body))

          (first= node 'while)
          (let [[_ pred & body] node]
            (ast/->WhileLoop pred body))

          (first= node 'let)
          (let [[_ binds & body] node]
            (ast/->LetBlock (map (fn [[decl value]]
                                   (ast/->Assignment
                                    (parse-var-declaration decl)
                                    (parse value))) (partition 2 binds)) body))

          (first= node 'defstruct)
          (let [[_ name & members] node]
            (ast/->StructDef name (map parse-var-declaration members)))

          (first= node 'sizeof)
          (let [[_ t] node]
            (ast/->FunctionCall 'sizeof [(parse-type t)]))

          (bit-operator? node)
          (let [[op & params] node]
            (ast/->ArithmeticOp (bit-operator->sym op) params))

          (function-call? node)
          (if (= 'return (first node))
            (ast/->Return (second node))
            (ast/->FunctionCall (first node) (rest node)))

          :else node)))

(defn parse [source]
  (transform-zipper-backwards (generic-zipper source) parse-position))

(defn parse-file [filename]
  (ast/->CodeFile
   (parse (util/read-edn filename))))

(ns splat.parser
  (:require [clojure.tools.reader.edn :as edn]
            [clojure.zip :as zip]
            [clojure.string :as str]
            [splat.ast :as ast]
            [splat.util :as util]))

(def operators #{'+ '- '/ '* '< '> '<= '>= '== '!=})

(def translated-operator->sym
  {'bit-or  (symbol "|")
   'bit-and (symbol "&")
   'bit-xor (symbol "^")
   'bit-not (symbol "~")
   '>>      (symbol ">>")
   '<<      (symbol "<<")
   'and     (symbol "&&")
   'or      (symbol "||")
   'not     (symbol "!")})

(defn translated-operator? [n]
  (and (list? n) (get (set (keys translated-operator->sym)) (first n))))

(defn- operator? [node]
  (and (list? node)
       (operators (first node))))

(defn- pre-directive? [node]
  (and (list? node)
       (let [x (first node)]
         (and (symbol? x) (str/starts-with? (name x) "!")))))

(defn- first= [coll item]
  (and (list? coll) (= item (first coll))))

(defn- function-call? [node]
  (and (list? node) (symbol? (first node))))

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

(defn- parse-node [z]
  (let [node (zip/node z)]
    (cond (operator? node)
          (ast/->OpApplication (first node) (rest node))

          (translated-operator? node)
          (let [[op & params] node]
            (ast/->OpApplication (translated-operator->sym op) params))

          (pre-directive? node)
          (ast/->PreDirective (first node) (rest node))

          (first= node 'do)
          (let [[_ & body] node]
            (ast/->Statements body))

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

          (first= node 'if)
          (let [[_ test then else] node]
            (ast/->IfThenElse test then else))
          
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

          (function-call? node)
          (if (= 'return (first node))
            (ast/->Return (second node))
            (ast/->FunctionCall (first node) (rest node)))

          :else node)))

(defn parse [source]
  (util/transform-zipper-backwards (util/generic-zipper source) parse-node))

(defn parse-source [source]
  (ast/->CodeFile (parse source)))

(defn parse-file [filename]
  (parse-source (util/read-edn filename)))

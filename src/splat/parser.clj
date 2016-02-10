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
   'not     (symbol "!")
   'comma   ","})

(defn translated-operator? [n]
  (and (seq? n) (get (set (keys translated-operator->sym)) (first n))))

(defn- operator? [node]
  (and (seq? node)
       (operators (first node))))

(defn- pre-directive? [node]
  (and (seq? node)
       (let [x (first node)]
         (and (symbol? x) (str/starts-with? (name x) "!")))))

(defn- first= [coll item]
  (and (seq? coll) (= item (first coll))))

(defn- function-call? [node]
  (and (seq? node) (symbol? (first node))))

(defn- var-name [s]
  (-> s str (str/replace "*" "") symbol))

(defn- pointer-name? [s]
  (-> s str (str/starts-with? "*")))

(defn- member-pointer-access? [node]
  (and (seq? node)
       (some-> node first str (str/starts-with? ".-"))))

(defn- member-access? [node]
  (and (seq? node)
       (some-> node first str (str/starts-with? "."))))

(defn- parse-int [s]
  (try (Integer/parseInt s)
       (catch Exception _ nil)))

(defn- parse-array-spec [s]
  (let [[_ size] (str/split s #"\.")]
    (or size :empty)))

(defn- parse-type-tokens [tokens]
  (loop [tokens tokens
         d      {:pointer-level 0
                 :arrays        []}]
    (if-not (seq tokens)
      d
      (recur
       (next tokens)
       (let [t (first tokens)]
         (if (str/starts-with? t "arr")
           (update d :arrays conj (parse-array-spec t))
           (condp = t
             "const"    (assoc d :const? true)
             "restrict" (assoc d :restrict? true)
             "volatile" (assoc d :volatile? true)
             "extern"   (assoc d :extern? true)
             "void"     (assoc d :void? true)
             "struct"   (assoc d :struct? true)
             "arr"      (update d :array-level inc)
             "ptr"      (update d :pointer-level inc)
             (update d :types conj t))))))))

(defn- parse-var-declaration [decl]
  (if ((some-fn ast/function-pointer?
                ast/member-access?
                ast/member-pointer-access?) decl)
    decl
    (let [tokens   (-> decl str (str/split #":"))
          var-name (var-name (first tokens))
          d        (ast/map->Declaration {:name var-name})
          t        (merge (ast/map->Type {}) (parse-type-tokens (rest tokens)))]
      (assoc d :type t))))

(defn parse-type [decl]
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

(defn- has-type? [s]
  (-> s str (str/includes? ":")))

(defn- parse-node [z]
  (let [node (zip/node z)]
    (cond (and (seq? node) (= node '()))
          (ast/->NoOp)

          (operator? node)
          (ast/->OpApplication (first node) (rest node))

          (translated-operator? node)
          (let [[op & params] node]
            (ast/->OpApplication (translated-operator->sym op) params))

          (pre-directive? node)
          (ast/->PreDirective (first node) (rest node))

          (member-pointer-access? node)
          (let [[member-name expr] node]
            (ast/->MemberPointerAccess (-> member-name str (subs 2) symbol) expr))

          (member-access? node)
          (let [[member-name expr] node]
            (ast/->MemberAccess (-> member-name str (subs 1) symbol) expr))

          (first= node 'do)
          (let [[_ & body] node]
            (ast/->Statements body))

          (first= node 'defn)
          (let [[_ decl params & body] node]
            (ast/->Function (parse-var-declaration decl) (parse-function-params params) body))

          (first= node 'fn)
          (let [[_ return params & body] node]
            (ast/->Lambda (parse-type return) (parse-function-params params) body))

          (first= node 'declare)
          (let [[_ decl] node]
            (parse-var-declaration decl))

          (first= node 'set!)
          (parse-assignment node)

          (first= node 'cast)
          (let [[_ type expr] node]
            (ast/->Cast (parse-type type) expr))

          (first= node 'deftype)
          (let [[_ decl] node]
            (if (or (symbol? decl) (string? decl))
              (ast/->DefType (parse-var-declaration decl))
              (ast/->DefType decl)))

          (first= node 'fn-ptr)
          (let [[_ return params] node]
            (ast/->FunctionPointer
             (parse-var-declaration return)
             (map parse-var-declaration params)))

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
            (when (> (count node) 4)
              (throw (ex-info "\"if\" can only have 3 expressions, test, then and else"
                              {:node node})))
            (ast/->IfThenElse test then else))

          (first= node 'let)
          (let [[_ binds & body] node]
            (ast/->LetBlock
             (map (fn [[decl value]]
                    (if (= '<none> value)
                      (parse-var-declaration decl)
                      (ast/->Assignment
                       (if-not (has-type? decl)
                         decl
                         (parse-var-declaration decl))
                       value))) (partition 2 binds)) body))

          (first= node 'defstruct)
          (let [[_ name & members] node]
            (ast/->StructDef name (map parse-var-declaration members)))

          (first= node 'sizeof)
          (let [[_ t] node]
            (ast/->FunctionCall 'sizeof [(parse-type t)]))

          (first= node 'return)
          (ast/->Return (second node))

          (first= node 'deref)
          (ast/->FunctionCall '* (rest node))

          (first= node 'ptr)
          (ast/->FunctionCall '& (rest node))

          (function-call? node)
          (ast/->FunctionCall (first node) (rest node))

          :else node)))

(defn parse [source]
  (util/transform-zipper-backwards (util/generic-zipper source) parse-node))

(defn parse-source [source]
  (ast/->CodeFile (parse source)))

(defn parse-file [filename]
  (parse-source (util/read-edn filename)))

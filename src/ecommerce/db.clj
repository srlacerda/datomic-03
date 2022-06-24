(ns ecommerce.db
  (:use clojure.pprint)
  (:require [datomic.api :as d]
            [ecommerce.model :as model]
            [schema.core :as s]
            [clojure.walk :as walk]))

(def db-uri "datomic:dev://localhost:4334/ecommerce")

(defn abre-conexao! []
  (d/create-database db-uri)
  (d/connect db-uri))

(defn apaga-banco! []
  (d/delete-database db-uri))

; Produtos
; id?
; nome String 1 ==> Computador Novo
; slug String 1 ==> /computador_novo
; preco ponto flutuantes 1 ==> 3500.10
; categoria_id integer ==> 3

; id_entidade atributo valor
; 15 :produto/nome Computador Novo     ID_TX     operacao
; 15 :produto/slug /computador_novo    ID_TX     operacao
; 15 :produto/preco 3500.10            ID_TX     operacao
; 15 :produto/categoria 37
; 17 :produto/nome Telefone Caro       ID_TX     operacao
; 17 :produto/slug /telefone           ID_TX     operacao
; 17 :produto/preco 8888.88            ID_TX     operacao

; 37 :categoria/nome Eletronicos

;[
; :db/add :produto/nome "Camiseta"
; :db/add :produto/slug "/camiseta"
; :db/add :produto/preco 30M
; ]
;#datom[72 :db/ident :produto/nome]
;#datom[73 :db/ident :produto/slug]
;#datom[74 :db/ident :produto/preco]
;
;[?transacao 50 "2019-01-01"]
;[_ _ _ ?transacao]
;#datom[13194139534324 50 #inst "2022-06-23T14:36:42.572-00:00" 13194139534324 true]
;#datom[17592186045429 72 "Camiseta" 13194139534324 true]
;#datom[17592186045429 73 "/camiseta" 13194139534324 true]
;#datom[17592186045429 74 30M 13194139534324 true]


(def schema [
             ;Produtos
             {:db/ident       :produto/nome
              :db/valueType   :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc         "O nome de um produto"}
             {:db/ident       :produto/slug
              :db/valueType   :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc         "O caminho para acessar esse produto via http"}
             {:db/ident       :produto/preco
              :db/valueType   :db.type/bigdec
              :db/cardinality :db.cardinality/one
              :db/doc         "O preco de um produto com precisao monetaria"}
             {:db/ident       :produto/palavra-chave
              :db/valueType   :db.type/string
              :db/cardinality :db.cardinality/many}
             {:db/ident       :produto/id
              :db/valueType   :db.type/uuid
              :db/cardinality :db.cardinality/one
              :db/unique      :db.unique/identity}
             {:db/ident       :produto/categoria
              :db/valueType   :db.type/ref
              :db/cardinality :db.cardinality/one}

             ;Categorias
             {:db/ident       :categoria/nome
              :db/valueType   :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident       :categoria/id
              :db/valueType   :db.type/uuid
              :db/cardinality :db.cardinality/one
              :db/unique      :db.unique/identity}

             ;Transacoes
             {:db/ident       :tx-data/ip
              :db/valueType   :db.type/string
              :db/cardinality :db.cardinality/one}

             ])


(s/defn adiciona-ou-altera-produtos!
  ([conn, produtos :- [model/Produto]]
   (d/transact conn produtos))
  ([conn, produtos :- [model/Produto], ip]
   (let [db-add-ip [:db/add "datomic.tx" :tx-data/ip ip]]
     (d/transact conn (conj produtos db-add-ip)))))

(defn cria-schema! [conn]
  (d/transact conn schema))

(defn dissoc-db-id [entidade]
  (if (map? entidade)
    (dissoc entidade :db/id)
    entidade))

(defn datomic-para-entidade [entidades]
  (walk/prewalk dissoc-db-id entidades))

; o maybe permite nil
; nil permite nullpointerexception
; nullpointerexception permite um inferno de exceptions
; usamos maybe SOMENTE em retorno de funcao
; E somente quando fizer muito sentido
; isto é... maybe nao e usado em mapas. em maps usamos as chaves opcionais
(s/defn um-produto :- (s/maybe model/Produto) [db, produto-id :- java.util.UUID]
  (let [resultado (d/pull db '[* {:produto/categoria [*]}] [:produto/id produto-id])
        produto (datomic-para-entidade resultado)]
    (if (:produto/id produto)
      produto
      nil)))

(s/defn um-produto! :- model/Produto [db, produto-id :- java.util.UUID]
  (let [produto (um-produto db produto-id)]
    (when (nil? produto)
      (throw (ex-info "Nao encontrei uma entidade"
                     {:type :errors/not-found, :id produto-id})))
    produto))

(defn db-adds-de-atribuicao-de-categorias [produtos categoria]
  (reduce (fn [db-adds produto] (conj db-adds [:db/add
                                               [:produto/id (:produto/id produto)]
                                               :produto/categoria
                                               [:categoria/id (:categoria/id categoria)]]))

          []
          produtos))

(defn atribui-categorias! [conn produtos categoria]
  (let [a-transacionar (db-adds-de-atribuicao-de-categorias produtos categoria)]
    ;(pprint "a-transacionar")
    ;(pprint a-transacionar)
    (d/transact conn a-transacionar)))

(s/defn adiciona-categorias! [conn, categorias :- [model/Categoria]]
  (d/transact conn categorias))



; com o [] no find, a query nao vai retornar tuplas + e com o ... no final a query vai retornar todos os registros
(s/defn todas-as-categorias :- [model/Categoria] [db]
  (datomic-para-entidade
    (d/q '[:find [(pull ?categoria [*]) ...]
         :where [?categoria :categoria/id]]
       db)))

;[{categoria}, {categoria}, {categoria}]
;[[{categoria}], [{categoria}], [{categoria}]]
;
;:find nome, preco
;[
; [nome, preco]
; [nome, preco]
; [nome, preco]
; ]
;
;:find nome
;[
; nome
; nome
; nome
; ]

(s/defn todos-os-produtos :- [model/Produto] [db]
  (datomic-para-entidade
    (d/q '[:find [(pull ?entidade [* {:produto/categoria [*]}]) ...]
         :where [?entidade :produto/nome]] db)))

(defn cria-dados-de-exemplo [conn]
  (def eletronicos (model/nova-categoria "Eletronicos"))
  (def esporte (model/nova-categoria "Esporte"))
  (pprint @(adiciona-categorias! conn [eletronicos, esporte]))

  (def computador (model/novo-produto (model/uuid) "Computador Novo", "/computador-novo", 2500.10M))
  (def celular (model/novo-produto (model/uuid) "Celular Caro", "/celular", 888888.10M))
  (def celular-barato (model/novo-produto "Celular Barato", "/celular-barato", 0.1M))
  (def xadrez (model/novo-produto "Tabuleiro de xadrez", "/tabuleiro-de-xadrez", 30M))
  (pprint @(adiciona-ou-altera-produtos! conn [computador, celular, celular-barato, xadrez] "200.216.222.125"))

  (atribui-categorias! conn [computador, celular, celular-barato] eletronicos)
  (atribui-categorias! conn [xadrez] esporte))




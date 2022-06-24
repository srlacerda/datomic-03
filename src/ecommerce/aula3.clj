(ns ecommerce.aula3
  (:use clojure.pprint)
  (:require [datomic.api :as d]
            [ecommerce.db :as db]
            [ecommerce.model :as model]
            [schema.core :as s]))

(s/set-fn-validation! true)

(db/apaga-banco!)
(def conn (db/abre-conexao!))
(db/cria-schema! conn)
(db/cria-dados-de-exemplo conn)

(def produtos (db/todos-os-produtos (d/db conn)))
(def primeiro-produto (first produtos))
(pprint primeiro-produto)

; se nao encontra, devolve nil
(pprint (db/um-produto (d/db conn) (:produto/id primeiro-produto)))
(pprint (db/um-produto (d/db conn)  (model/uuid)))

; se nao encontra, joga um erro, independentemente de schema ativo ou nao
(pprint (db/um-produto! (d/db conn) (:produto/id primeiro-produto)))
(pprint (db/um-produto! (d/db conn) (model/uuid)))



(ns ecommerce.aula2
  (:use clojure.pprint)
  (:require [datomic.api :as d]
            [ecommerce.db :as db]
            [ecommerce.model :as model]
            [schema.core :as s]))

(s/set-fn-validation! true)
;(s/set-fn-validation! false)

(db/apaga-banco!)
(def conn (db/abre-conexao!))
(db/cria-schema! conn)
(db/cria-dados-de-exemplo conn)

(pprint (db/todas-as-categorias (d/db conn)))
(pprint (db/todos-os-produtos (d/db conn)))

(def dama {:produto/nome  "Dama"
           :produto/slug  "/dama"
           :produto/preco 15.5M
           :produto/id    (model/uuid)})
(db/adiciona-ou-altera-produtos! conn [dama])
;(pprint (db/um-produto (d/db conn) (:produto/id dama)))
;
;; update/insert ==? upsert
;(db/adiciona-ou-altera-produtos! conn [(assoc dama :produto/slug "/jogo-de-dama")])
;(pprint (db/um-produto (d/db conn) (:produto/id dama)))
;
;(db/adiciona-ou-altera-produtos! conn [(assoc dama :produto/preco 150.5M)])
;(pprint (db/um-produto (d/db conn) (:produto/id dama)))

;detectamos um problema que e uma dificuldade... entender que updates sobrescreverao campos e operacoes anteriores

(defn atualiza-preco []
  (println "atualizando preco")
  (let [produto (db/um-produto (d/db conn) (:produto/id dama))
        produto (assoc produto :produto/preco 999M)]
    (db/adiciona-ou-altera-produtos! conn [produto])
    (println "Atualizado preco")
    produto))

(defn atualiza-slug []
  (println "atualizando slug")
  (let [produto (db/um-produto (d/db conn) (:produto/id dama))]
    (Thread/sleep 3000)
    (let [produto (assoc produto :produto/slug "/jogo-de-dama-carinho")]
      (db/adiciona-ou-altera-produtos! conn [produto])
      (println "Atualizado slug")
      produto)))

(pprint dama)

(defn roda-transacoes [tx]
  (let [futuros (mapv #(future (%)) tx)]
    (pprint (map deref futuros))
    (pprint "Resultado final")
    (pprint (db/um-produto (d/db conn) (:produto/id dama)))))

(roda-transacoes [atualiza-preco atualiza-slug])



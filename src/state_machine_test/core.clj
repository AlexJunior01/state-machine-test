(ns state-machine-test.core
  (:require [tilakone.core :as tk :refer [_]]
            [state-machine-test.state-machine :as st])
  (:import (java.util UUID)))

(def signals-validar-compra
  [:init :user-info :credit-card :aprovacao-limite :anti-fraude :report])

(defn is-fraud?
  [buy]
  (> (:price buy) 100000))

(defmulti actions (fn [signal fsm]
                    (println "ACTION:" signal)
                    signal))


(defmethod actions :user-info
  [_ fsm]
  (clojure.pprint/pprint fsm)
  (let [keys-set #{:name :doc :id}
        user (get-in fsm [::tk/process :user])]
    (if (= keys-set (-> user keys set))
      fsm
      (->
        fsm
        (assoc-in [::tk/process :user] nil)
        (assoc-in [::tk/process :error] {:reason "Informações incompletas"})))))

(defmethod actions :credit-card
  [_ fsm]
  fsm)

(defmethod actions :aprovacao-limite
  [_ fsm]
  fsm)

(defmethod actions :anti-fraude
  [_ fsm]
  (let [buy (get-in fsm [::tk/process :buy])]
    (if (is-fraud? buy)
      (assoc-in fsm [::tk/process :fraud-identification] {:fraud? true :id    (UUID/randomUUID)})
      (assoc-in fsm [::tk/process :fraud-identification] {:fraud? false :id    (UUID/randomUUID)}))))

(defmethod actions :report
  [_ fsm]
  (assoc-in fsm [::tk/process :report] {:status "success"}))

(defmethod actions :error
  [_ fsm]
  (let [error (get-in fsm [::tk/process :error])
        signal (::tk/signal fsm)]
    (println (format "Error in %s: %s" signal error))
    (assoc-in fsm [::tk/process :report] {:status "error"
                                          :stage  signal
                                          :reason (:reason error)})))

(defmethod actions :default
  [_ fsm]
  fsm)

(defmulti guardian (fn [guard fsm]
                     (println "GUARDIAN:" (get-in fsm [::tk/signal]))
                     guard))

(defmethod guardian :user-info
  [guard fsm]
  (if (get-in fsm [::tk/process :user])
    true
    false))

(defmethod guardian :credit-card
  [guard fsm]
  true)

(defmethod guardian :aprovacao-limite
  [guard fsm]
  true)

(defmethod guardian :anti-fraude
  [guard fsm]
  (let [fraud (get-in fsm [::tk/process :fraud-identification])]
    (if (:fraud? fraud)
      false
      true)))

(defmethod guardian :error
  [guard fsm]
  true)

(defmethod guardian :enter-guard
  [guard fsm]
  true)

(defmethod guardian :leave-guard
  [guard fsm]
  true)

(defmethod guardian :default
  [guard fsm]
  true)


(defn validate-purchase
  "Recebe os campos user, card e buy para fazer a verificação da compra"
  [payload]
  (let [process {::tk/states  st/estados-validar-comprar
                 ::tk/state   :start
                 ::tk/action! (fn [{::tk/keys [signal action] :as fsm}] ;; Usando o valor :action para definir quais actions chamar
                                (actions action fsm))
                 ::tk/guard?  (fn [{::tk/keys [signal guard] :as fsm}] ;; Usando o signal para definir qual guardian chamar
                                (guardian guard fsm))

                 ;; state
                 :user        (:user payload)
                 :credit-card (:card payload)
                 :buy         (:buy payload)}
        result (reduce tk/apply-signal process signals-validar-compra)
        report (get-in result [:report])]
    report))


(comment
  ;; Testando com valores para passar por todos os casos
  (let [user {:name "alex" :doc "40368554848" :id 1}
        credit-card {:number "123412341234" :limit 5000}
        compra {:price 1000 :id 1}]
    (validate-purchase {:user user :card credit-card :buy compra}))

  ;info de usuário errado
  ;; Erro com estado de error definido
  (let [user {:name "alex"}
        credit-card {:number "123412341234" :limit 5000}
        compra {:valor 1000}]
    (validate-purchase {:user user :card credit-card :buy compra}))

  ;erro em fraude
  ; Erro sem definir um estado de erro
  (let [user {:name "alex" :doc "12345678910" :id 1}
        credit-card {:number "123412341234" :limit 5000}
        compra {:price 200000}]
    (validate-purchase {:user user :card credit-card :buy compra})))
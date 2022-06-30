(ns state-machine-test.actions-order
  (:require [tilakone.core :as tk :refer [_]]))

(def signals
  [:S0 :a1 :S1 :S2 :S3])

(def estados
  [{::tk/name :start
    ::tk/desc "Estado inicial."
    ::tk/transitions [{::tk/on :S0
                       ::tk/to :validate-credit-card}]}

   {::tk/name :validate-credit-card
    ::tk/desc "Verifica se o limite está disponível."
    ::tk/transitions [{::tk/on :S1
                       ::tk/to :check-limit
                       ::tk/guards [:t1]
                       ::tk/actions [:t1]}]
    ::tk/enter {::tk/guards [:enter1]
                ::tk/actions [:enter1]}
    ::tk/leave {::tk/guards [:leave1]
                ::tk/actions [:leave1]}}

   {::tk/name :check-limit
    ::tk/desc "Verifica se o limite está disponível."
    ::tk/transitions [{::tk/on :S2
                       ::tk/to :final-report
                       ::tk/guards [:t2]
                       ::tk/actions [:t2]}]
    ::tk/enter {::tk/guards [:enter2]
                ::tk/actions [:enter2]}
    ::tk/leave {::tk/guards [:leave2]
                ::tk/actions [:leave2]}}

   {::tk/name :final-report
    ::tk/desc "Realiza o report final."
    ::tk/transitions [{::tk/on _}]}

   {::tk/name :error
    ::tk/desc "Valida as informações passadas pelo cliente."
    ::tk/transitions [{::tk/on _}]}])


(defmulti actions (fn [signal fsm]
                    signal))


(defmethod actions :default
  [action fsm]
  (let [signal (get-in fsm [::tk/signal])]
    (println (format "ACTION %s - SIGNAL %s" action  signal)))
  fsm)

(defmulti guardian (fn [guard fsm]
                     guard))

(defmethod guardian :default
  [guard fsm]
  (let [signal (get-in fsm [::tk/signal])]
    (println (format "GUARD %s - SIGNAL %s"  guard  signal)))
  true)


(defn test-order
  "Máquina de estado simples com objetivo de testar a ordem de ações/guardians
  e em quais momentos os signals são trocados"
  []
  (let [process {::tk/states  estados
                 ::tk/state   :start
                 ::tk/action! (fn [{::tk/keys [signal action] :as fsm}] ;; Usando o valor :action para definir quais actions chamar
                                (actions action fsm))
                 ::tk/guard?  (fn [{::tk/keys [signal guard] :as fsm}] ;; Usando o signal para definir qual guardian chamar
                                (guardian guard fsm))}
        result (reduce tk/apply-signal process signals)
        report (get-in result [:S3])]
    report))




(comment
  (test-order))
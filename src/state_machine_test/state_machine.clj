(ns state-machine-test.state-machine
  (:require [tilakone.core :as tk :refer [_]]))

;[{::name        Any ;                     State name (can be string, keyword, symbol, any clojure value)
;  ::desc        Str ;                     Optional state description
;  ::transitions [{::name    Any ;         Transition name
;                  ::desc    Str ;         Transition description
;                  ::to      Any ;         Name of the next state
;                  ::on      Matcher ;     Data for match?, does the signal match this transition?
;                  ::guards  [Guard] ;     Data for guard?, is this transition allowed?
;                  ::actions [Action]}] ;  Actions to be performed on this transition
;  ; Guards and actions used when state is transferred to this stateP
;  ::enter       {::guards  [Guard]
;                 ::actions [Action]}
;  ; Guards and actions used when state is transferred from this state:
;  ::leave       {::guards  [Guard]
;                 ::actions [Action]}
;  ; Guards and actions used when state transfer is not made:
;  ::stay        {::guards  [Guard]
;                 ::actions [Action]}}]

(def estados-validar-comprar
  [{::tk/name :start
    ::tk/desc "Estado inicial."
    ::tk/transitions [{::tk/on :init
                       ::tk/to :validate-user-info}

                      {::tk/on _
                       ::tk/to :error}]}

   {::tk/name :validate-user-info
    ::tk/desc "Valida as informações do usuário."
    ::tk/transitions [{::tk/on :user-info
                       ::tk/guards [:user-info]
                       ::tk/to :validate-credit-card}

                      {::tk/on _
                       ::tk/to :error}]
    ::tk/enter {::tk/actions [:user-info]}}


   {::tk/name :validate-credit-card
    ::tk/desc "Valida as informações do cartão de crédito."
    ::tk/transitions [{::tk/on :credit-card
                       ::tk/to :check-limit
                       ::tk/guards [:credit-card]}

                      {::tk/on _
                       ::tk/to :error}]
    ::tk/enter {::tk/actions [:credit-card]}}

   {::tk/name :check-limit
    ::tk/desc "Verifica se o limite está disponível."
    ::tk/transitions [{::tk/on :aprovacao-limite
                       ::tk/to :anti-fraud-system
                       ::tk/guards [:aprovacao-limite]}

                      {::tk/on _
                       ::tk/to :error}]
    ::tk/enter {::tk/actions [:aprovacao-limite]}}

   {::tk/name :anti-fraud-system
    ::tk/desc "Passa a compra pelo modelo anti-fraude."
    ::tk/transitions [{::tk/on :anti-fraude
                       ::tk/to :final-report
                       ::tk/guards [:anti-fraude]}]
    ::tk/enter {::tk/actions [:anti-fraude]}}

   {::tk/name :final-report
    ::tk/desc "Realiza o report final."
    ::tk/transitions [{::tk/on _}]
    ::tk/enter {::tk/actions [:report]}}

   {::tk/name :error
    ::tk/desc "Valida as informações passadas pelo cliente."
    ::tk/transitions [{::tk/on _}]
    ::tk/enter {::tk/actions [:error]}}])
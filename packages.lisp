(in-package :common-lisp-user)

(defpackage :jabber
  (:use :cl :cl-user)
  (:export
   ;; connection class and accessors
   :connection
   :connection-router-host :connection-router-port
   :connection-stanza-handler :connection-socket :connection-iq-get
   :connection-iq-set :connection-authenticated-p
   ;; connection methods
   :connect :send-start-tag :authenticate :authenticated
   :connection-close :read-and-act :handle-stanza
   :default-stanza-handler :send-stanza :send-iq :send-iq-result
   :send-stanza-error :send-message
   ;; restarts
   :drop-stanza :return-error-stanza
   ;; component class and accessors
   :component
   :component-jid :component-secret
   ;; client class and accessors
   :client
   :client-username :client-server :client-resource :client-password
   :client-register-p :client-actual-jid
   ;; muc class and accessors
   :muc
   :muc-conferences
   :join-conference
   :get-conference
   :send-muc-message
   ;; conference class and accessors
   :conference
   :joined-conference
   :conference-server
   :remove-participant
   :new-participant
   :handle-conference-presence
   :conference-participants :conference-name :conference-nick :conference-handler
   :participant-nick :participant-real-jid
   ;; conditions and errors
   :unexpected-stanza-error :jabber-error :auth-error
   :auth-stanza-error
   :jabber-error-condition :text :jabber-error-type :app-specific
   :conference-destroyed-error
   ;; stanza classes and methods
   :stanza :message :iq :presence
   :stanza-name :stanza-from :stanza-to :stanza-type :stanza-id
   :stanza-node :stanza-children :parse-stanza :message-body :iq-xmlns
   :find-stanza-error :stanza-error-condition :stanza-error-type
   :stanza-error-text :stanza-error-app-specific :signal-stanza-error
   :normal-from :normal-to
   ;; utility functions
   :jid-username :jid-user :jid-server :jid-resource :jid-normalize :jid-equal
   :normalize-username :normalize-hostname :normalize-resource
   ;; disco functions
   :make-disco-info :make-disco-items :parse-disco-info :parse-disco-items
   ;; disco handling connection
   :disco-handling-connection :disco-items :disco-info :get-disco-info :get-disco-items
   :add-disco-info :add-disco-items
   ;; register functions
   :make-register-get-form :read-register-set-form
   ;; x:data classes, methods and functions
   :xform
   :xform-type :xform-title :xform-instructions :xform-fields
   :xform-items
   :xform-field
   :xform-field-desc :xform-field-label :xform-field-var
   :xform-field-required :xform-field-values :xform-field-options
   :xform-field-type :parse-xdata-form :output-xdata-form :find-field
   ;; ad-hoc commands
   :adhoc-handling-connection :add-adhoc-command
   ;; roster
   :roster-handling-client :get-roster-plist :have-subscription-to
   :have-subscription-from :got-roster :change-roster-plist
   :remove-roster-item :get-roster-name
   ;; namespaces
   :+adhoc-namespace+ :+disco-info-namespace+ :+disco-items-namespace+
   :+roster-namespace+))

;; arch-tag: 7beff346-9594-11d9-831e-000a95c2fcd0

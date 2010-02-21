;;; Client

(in-package :jabber)

(defclass client (connection)
  ((router-port :accessor connection-router-port
                :initarg :router-port
                :initform 5222)
   (username    :accessor client-username
                :initarg :username
                :initform "foo")
   (server      :accessor client-server
		:initarg :server
		:initform "localhost")
   ;;resource is currently ignored
   (resource    :accessor client-resource
                :initarg :resource
                :initform "Lisp Jabber")
   (password    :accessor client-password
                :initarg :password
                :initform "secret")
   ;; Register account through JEP-0077?
   (register-p  :accessor client-register-p
		:initarg :register-p
		:initform nil)

   (cleartext   :accessor cleartext
		:initarg :cleartext
		:initform nil
		:documentation "Allow sending password in clear text.")
   (anonymous   :accessor anonymous
		:initarg :anonymous
		:initform nil
		:documentation "Use anonymous access.")
   
   ;; Our full JID, as decided by the server
   (actual-jid  :accessor client-actual-jid)

   ;; internal slots
   (sasl-client :accessor client-sasl-client)
   (sasl-authenticated-p :accessor client-authenticated-p
		    :initform nil)))

(defmethod send-start-tag ((c client))
  (format (connection-socket c)
	  "<stream:stream xmlns='jabber:client'
           xmlns:stream='http://etherx.jabber.org/streams'
           to='~a' version='1.0'>~%" (client-server c)))

(defmethod authenticate ((c client))
  ;; This is called every time we receive a new start tag.  Only
  ;; the first time we need to "authenticate" in the strict sense
  ;; of the word.
  ;; 
  ;; Anyway, we need to wait for the stream features element.
  (setf (connection-stanza-handler c)
	(cond
	 ((client-register-p c) 'get-stream-features-for-register)
	 ((not (client-authenticated-p c)) 'get-stream-features-for-auth)
	 (t 'get-stream-features-for-bind))))

(defmethod get-stream-features-for-auth ((c client) features)
  (unless (equal (xmls:node-name features) "features")
    (error 'unexpected-stanza-error :stanza features))

  (let* ((mechanisms (find "mechanisms" 
			   (xmls:node-children features)
			   :key #'xmls:node-name
			   :test #'string=))
	 (mechanism-list
	  (mapcar
	   #'(lambda (mechanism)
	       (car (xmls:node-children mechanism)))
	   (xmls:node-children mechanisms)))
	 (chosen-mechanism 
	  (sasl:choose-mechanism mechanism-list :cleartext (cleartext c)
				 :anonymous (anonymous c))))
    (unless chosen-mechanism
      (error "Couldn't choose mechanism (server offered ~A)." mechanism-list))
    (setf (client-sasl-client c) 
	  (make-instance chosen-mechanism
			 :authentication-id (client-username c)
			 :password (client-password c)
			 :service "xmpp"
			 :host (client-server c)))
    (let ((first-response (sasl:client-step (client-sasl-client c) nil)))
      (send-stanza c
		   (xmls:make-node :name "auth"
				   :ns "urn:ietf:params:xml:ns:xmpp-sasl"
				   :attrs (list
					   (list "mechanism" (sasl:mechanism-name (client-sasl-client c))))
				   :child (cl-base64:usb8-array-to-base64-string
					   first-response))))
    (setf (connection-stanza-handler c) 'sasl-authentication)))

(defmethod sasl-authentication ((c client) stanza)
  (cond
   ((equal (xmls:node-name stanza) "success")
    ;; Yay!  Let's just check that the server isn't cheating...
    (if (not (eql (sasl:client-step (client-sasl-client c) :success) :success))
	(error 'auth-error :text "Server reported success, but SASL library didn't agree"))

    (setf (connection-stanza-handler c) #'default-stanza-handler)
    (setf (connection-start-tag c) nil)
    (setf (client-authenticated-p c) t)
    (send-start-tag c))

   ((equal (xmls:node-name stanza) "failure")
    ;; XXX: parse SASL error
    (error 'auth-error :text stanza))

   ((equal (xmls:node-name stanza) "challenge")
    (let* ((decoded-challenge
	    (cl-base64:base64-string-to-usb8-array
	     (car (xmls:node-children stanza))))
	   (response (sasl:client-step (client-sasl-client c)
					decoded-challenge)))
      (if (eql response :failure)
	  (error 'auth-error :text "SASL authentication failed")
	(send-stanza c
		     (xmls:make-node :name "response"
				     :ns "urn:ietf:params:xml:ns:xmpp-sasl"
				     :child (cl-base64:usb8-array-to-base64-string
					     response))))))

   (t
    (error 'unexpected-stanza-error :stanza stanza))))

(defmethod get-stream-features-for-bind ((c client) features)
  (unless (and (equal (xmls:node-name features) "features")
	       (find "bind" (xmls:node-children features) :key 'xmls:node-name :test 'string=)
	       (find "session" (xmls:node-children features) :key 'xmls:node-name :test 'string=))
    (error 'unexpected-stanza-error :stanza features))

  ;; Strictly speaking, we want the ordinary IQ framework for
  ;; resource binding and session establishment.  Hopefully won't
  ;; hurt letting all of it in.
  (setf (connection-stanza-handler c) #'default-stanza-handler)

  (send-iq c
	   (xmls:make-node :name "bind"
			   :ns "urn:ietf:params:xml:ns:xmpp-bind")
	   :type "set"
	   :callback #'handle-resource-bind))

(defmethod handle-resource-bind ((c client) (i iq))
  (unless (equal (stanza-type i) "result")
    (signal-stanza-error 'auth-stanza-error (find-stanza-error i)))

  (setf (client-actual-jid c)
	(car (xmls:node-children
	      (find "jid"
		    (xmls:node-children (car (stanza-children i)))
		    :key 'xmls:node-name :test 'string=))))

  (send-iq c
	   (xmls:make-node :name "session"
			   :ns "urn:ietf:params:xml:ns:xmpp-session")
	   :type "set"
	   :callback #'handle-session-established))

(defmethod handle-session-established ((c client) (i iq))
  (unless (equal (stanza-type i) "result")
    (signal-stanza-error 'auth-stanza-error (find-stanza-error i)))

  ;;Finally done!
  (authenticated c))

(defmethod get-stream-features-for-register ((c client) features)
  (unless (equal (xmls:node-name features) "features")
    (error 'unexpected-stanza-error :stanza features))

  (unless (find '("register" . "http://jabber.org/features/iq-register")
		(xmls:node-children features)
		:test 'equal :key 'first)
    (error 'auth-error :text "In-band registration not supported"))

  ;; Need IQ handling framework
  (setf (connection-stanza-handler c) #'default-stanza-handler)

  ;; According to the protocol, we should here query for required
  ;; fields in the registration.  But we don't do that, as we have no
  ;; way of asking the user for additional information.
  (send-iq c
	   (xmls:make-node :name "query" :ns "jabber:iq:register"
			   :children
			   (list
			    (xmls:make-node :name "username"
					    :child (client-username c))
			    (xmls:make-node :name "password"
					    :child (client-password c))))
	   :type "set"
	   :callback #'handle-registration))

(defmethod handle-registration ((c client) (i iq))
  (unless (equal (stanza-type i) "result")
    (signal-stanza-error 'auth-stanza-error (find-stanza-error i)))

  ;; We're done.
  (authenticated c)
  ;; And disconnect, as there's no easy way to continue with SASL
  ;; authentication from here.
  (connection-close c))

;;; arch-id: 638d5853-1b79-4869-b57e-38640cdf385f

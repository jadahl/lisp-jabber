;;; Quite basic implementation of JEP-0114 (accept flavour) et.al.
;;; Works with CLISP (and maybe GCL).

(in-package :jabber)

(defclass component (connection)
  ((router-port :accessor connection-router-port
                :initarg :router-port
                :initform 5347)
   (jid         :accessor component-jid
                :initarg :jid
                :initform "foo.localhost")
   (secret      :accessor component-secret
                :initarg :secret
                :initform "secret")))


(defmethod send-start-tag ((c component))
  ;; Send start tag, with newline to please CMUCL
  (format (connection-socket c) "<stream:stream 
          xmlns='jabber:component:accept'
          xmlns:stream='http://etherx.jabber.org/streams'
          to='~A'>~%" (component-jid c)))

;(defmethod authenticate ((c client))
;  (format (connection-socket c) "<stream:stream
;          xmlns='jabber:client'
;          xmlns:stream='http://etherx.jabber.org/streams'
;          to='~A'>" (server-part (component-jid c)))


(defmethod authenticate ((c component))
  ;; Authenticate
  (let* ((id (second (assoc "id" (xmls:node-attrs (connection-start-tag c)) :test #'string=)))
         (handshake-hash (concatenate 'list
                                      (sb-sha1:sha1sum-sequence
                                        (concatenate 'string
                                                     id (component-secret c)))))
         ;; is there any easier way to get lowercase hexadecimals?
         (hex-hash (apply #'concatenate 'string
                          (mapcar #'(lambda (byte)
                                      (string-downcase (format nil "~2,'0x" byte)))
                                  handshake-hash))))
    (send-stanza c (xmls:make-node 
                      :name "handshake"
                      :ns "jabber:component:accept"
                      :child hex-hash)))
  ;; wait for handshake
  (setf (connection-stanza-handler c) 'authenticate2))



;; Receive start tag (hopefully)
;;(let ((start-tag (xmls:parse *router-connection* :start-tag-only t)))

(defmethod authenticate2 ((c component) response)
  (unless (equal (xmls:node-name response) "handshake")
    ;; XXX: parse stream error
    (error 'auth-error :text response))
  (setf (connection-stanza-handler c) #'default-stanza-handler)
  (authenticated c))

;;; arch-tag: 2309f322-efc9-45f4-8a0e-633b1a140735

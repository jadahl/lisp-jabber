;;; Quite basic implementation of JEP-0114 (accept flavour) et.al.
;;; Works with CLISP (and maybe GCL).

(in-package :jabber)

(defclass connection ()
  ((router-host :accessor connection-router-host
                :initarg :router-host)
   (router-port :accessor connection-router-port
                :initarg :router-port)
   (stanza-handler :accessor connection-stanza-handler
                   :initarg :stanza-handler
                   :initform nil)
   (socket      :accessor connection-socket)
   (start-tag :accessor connection-start-tag
              :initform nil)
   (authenticated-p :accessor connection-authenticated-p
		    :initform nil)
   ;; maps IDs of sent IQs to callbacks
   (iq-table :accessor connection-iq-table
	     :initform (make-hash-table :test #'equal))
   ;; the following two map namespaces to IQ handlers
   (iq-get :accessor connection-iq-get
	   :initform (make-hash-table :test #'equal))
   (iq-set :accessor connection-iq-set
	   :initform (make-hash-table :test #'equal))))

(defmethod connect ((c connection))
  ;; Connect to router
  (let ((port (connection-router-port c))
        (host (connection-router-host c)))
    (setf (connection-authenticated-p c) nil)
    (setf (connection-socket c)
	  #+clisp (socket:socket-connect port host
					 :element-type 'character
					 :external-format 'charset:utf-8)
	  #+sbcl (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
					      :type :stream :protocol :tcp)))
		   (sb-bsd-sockets:socket-connect socket
						  (sb-bsd-sockets:host-ent-address
						   (sb-bsd-sockets:get-host-by-name host))
						  port)
		   (sb-bsd-sockets:socket-make-stream
		    socket :input t :output t :buffering :none
		    :element-type 'character :external-format :utf-8))
	  #-(or sbcl clisp) (port:open-socket host port)
	  ;; XXX: unicode?
;;           #+gcl (si::socket port :host host)
	  )
    (send-start-tag c)))

(defmethod send-start-tag :before ((c connection)))

(defmethod authenticate :before ((c connection)))

(defmethod authenticated ((c connection))
  "This method is called when the connection is authenticated.
It should probably setup a stanza handler."
  (setf (connection-authenticated-p c) t))

(defmethod connection-close ((c connection))
  "Close component connection.
If connection is already closed, do nothing."
  (when (connection-socket c)
    #+gcl (si::close (connection-socket c))
    #-gcl (close (connection-socket c))
    (setf (connection-socket c) nil)))

(defmethod read-incoming ((c connection) &key (block nil))
  "Read incoming stanza.
If BLOCK is nil, return nil if no input is available."
  (when (or block
          (listen (connection-socket c)))
    (format t "data available... reading~%")
    (xmls:parse (connection-socket c) :start-tag-only (not (connection-start-tag c)))))

(defvar *print-xml-traffic* nil
  "If true, print all XML stanzas sent and received.")

(defmethod read-and-act ((c connection) &key (block nil))
  (let ((stanza (read-incoming c :block block)))
    (when stanza
      (when *print-xml-traffic* (format t "~&~S~%" stanza))
      (if (connection-start-tag c)
        (when (connection-stanza-handler c)
	  (restart-case
	   (funcall (connection-stanza-handler c) c stanza)
	   (drop-stanza 
	    () :report "Drop stanza and continue"
	    nil)
	   (return-error-stanza
	    () :report "Return an error stanza and continue"
	    (let ((parsed-stanza (parse-stanza stanza)))
	      (send-stanza-error c parsed-stanza
				 :type "wait"
				 :condition "internal-server-error")))))
        (progn
          (unless (equal (first stanza) '("stream" . "http://etherx.jabber.org/streams"))
            (error 'unexpected-stanza-error :stanza stanza))
          (setf (connection-start-tag c) stanza)
          (authenticate c)
          (read-and-act c :block block))))))

(defmethod handle-stanza ((c connection) (m message))
  )

(defmethod handle-stanza ((c connection) (p presence))
  )

(defmethod default-stanza-handler ((c connection) xml-data)
  (let ((stanza (parse-stanza xml-data)))
    (handle-stanza c stanza)))

(defmethod handle-stanza ((c connection) (i iq))
  (cond
   ((member (stanza-type i) '("result" "error") :test #'string=)
    (standard-iq-result-handler c i))
   ((string= (stanza-type i) "get")
    (let ((handler (gethash (iq-xmlns i) (connection-iq-get c))))
      (if handler
	  (funcall handler c i)
	(send-stanza-error c i
			   :type "cancel"
			   :condition "service-unavailable"))))
   ((string= (stanza-type i) "set")
    (let ((handler (gethash (iq-xmlns i) (connection-iq-set c))))
      (if handler
	  (funcall handler c i)
	(send-stanza-error c i
			   :type "cancel"
			   :condition "service-unavailable"))))))

(defmethod standard-iq-result-handler ((c connection) (i iq))
  "Dispatch incoming IQ results and errors to registered callbacks."
  (let ((callback (gethash (stanza-id i) (connection-iq-table c))))
    (when callback
      (remhash (stanza-id i) (connection-iq-table c))
      (funcall callback c i))))

(defvar iq-sequence-number 0)

(defmethod send-stanza ((c connection) stanza)
  "Send STANZA over the connection.
STANZA is an XML node as generated by XMLS:MAKE-NODE."
  (when *print-xml-traffic* (format t "~&sending ~A~%" (xmls:toxml stanza :entitify-nonascii nil)))
  (xmls:write-xml stanza
		  (connection-socket c)
		  :entitify-nonascii nil)
  ;; PORT opens line-buffered sockets in CMUCL by default,
  ;; and this newline won't hurt the others.
  (princ #\Newline (connection-socket c)))

(defmethod send-iq ((c connection) query &key from to type id callback)
  "Send QUERY (possibly nil) as payload of IQ stanza.
If ID is nil, an id is generated.
If CALLBACK is non-nil, it is added to the IQ callback table."
  ;; Only generate an id for "get" and "set" requests.  If this is
  ;; a "result" or "error" in response to a request without id (invalid
  ;; according to RFC 3920, but still occurs), let's not increase the
  ;; confusion.
  (when (null id)
    (if (member type '("get" "set") :test #'string=)
	(setq id (format nil "~a" (incf iq-sequence-number)))
      (warn "Sending IQ ~A without id attribute from ~A to ~A: ~A" type from to query)))
  (when callback
    (setf (gethash id (connection-iq-table c)) callback))
  (let (attrs)
    (when from
      (push (list "from" from) attrs))
    (when to
      (push (list "to" to) attrs))
    (push (list "type" type) attrs)
    (when id				;strictly speaking incorrect... see above
      (push (list "id" id) attrs))
    (send-stanza c (xmls:make-node :name "iq"
				   :attrs attrs
				   :child query))))

(defmethod send-message ((c connection) &key from to body subject type id thread additional-children)
  (let (attrs children)
    (flet ((maybe-push-attr (key value)
			    (when value
			      (push (list key value) attrs)))
	   (maybe-push-child (key value)
			     (when value
			       (push (xmls:make-node :name key :child value) children))))
      (mapc #'maybe-push-attr '("from" "to" "id" "type") (list from to id type))
      (mapc #'maybe-push-child '("subject" "body" "thread") (list subject body thread))
      (send-stanza c (xmls:make-node :name "message"
				     :attrs attrs
				     :children (append children additional-children))))))

(defmethod send-iq-result ((c connection) (in-response-to iq) &key payload)
  "Send a response to IN-RESPONSE-TO with payload PAYLOAD.
The stanza to send will have type \"result\", and \"from\" and \"to\"
values swapped compared to IN-RESPONSE-TO, and the same id."
  (send-iq c payload
	   :from (stanza-to in-response-to)
	   :to (stanza-from in-response-to)
	   :type "result"
	   :id (stanza-id in-response-to)))

(defmethod send-stanza-error ((c connection) (in-response-to stanza)
			      &key (keep-payload t) (type "modify")
			      (condition "bad-request") text app-specific)
  "Send an error reponse to IN-RESPONSE-TO.
If KEEP-PAYLOAD is t, include the sent data in the error."
  (let (attrs children error-children)
    (push (list "from" (stanza-to in-response-to)) attrs)
    (push (list "to" (stanza-from in-response-to)) attrs)
    (when (stanza-id in-response-to)
      (push (list "id" (stanza-id in-response-to)) attrs))
    (push (list "type" "error") attrs)

    (push (xmls:make-node :name condition
			  :ns "urn:ietf:params:xml:ns:xmpp-stanzas")
	  error-children)
    (when text
      (push (xmls:make-node :name "text"
			    :ns "urn:ietf:params:xml:ns:xmpp-stanzas"
			    :child text)
	    error-children))
    (when app-specific
      (push app-specific error-children))

    (when keep-payload
      (setq children (stanza-children in-response-to)))
    (push (xmls:make-node :name "error"
				   :attrs (list (list "type" type))
				   :children error-children) 
	  children)

    (let ((write-this
	   (xmls:make-node :name (stanza-name in-response-to)
			   :attrs attrs
			   :children children)))
      (send-stanza c write-this))))

;;; arch-tag: 699a3631-146e-4896-8d74-748626f545d1

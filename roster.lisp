(in-package :jabber)

(defparameter +roster-namespace+ "jabber:iq:roster"
  "The namespace of roster queries.")

(defclass roster-handling-client (client)
  ((roster :accessor roster
	   :initform (make-hash-table :test #'equal)
	   :documentation "Hash table for roster.
Keys are strings, normalized JIDs.
Values are plists.  Keys used:

:NAME          A string, or nil/absent
:SUBSCRIPTION  One of :NONE, :FROM, :TO or :BOTH
:ASK           :SUBSCRIBE, or absent
:GROUP         List of strings")
   (got-roster :accessor got-roster :initform nil
	       :documentation "True when client has received initial roster."))
  (:documentation
   "This is a base class keeping track of roster state."))

(defmethod authenticated :after ((c roster-handling-client))
  "Add roster handler, and request roster."
  (let ((set-table (connection-iq-set c)))
    (setf (gethash +roster-namespace+ set-table) #'handle-roster))
  (send-iq c (xmls:make-node :name "query"
			     :ns "jabber:iq:roster")
	   :type "get"
	   :callback #'handle-roster))

(defmethod handle-roster ((c roster-handling-client) (i iq))
  (let ((from (stanza-from i)))
    ;; RFC 3921, 7.2:
    ;; ... specifically, the stanza MUST either have no 'from' attribute
    ;; (i.e., implicitly from the server) or have a 'from' attribute
    ;; whose value matches the user's bare JID (of the form
    ;; <user@domain>) or full JID (of the form
    ;; <user@domain/resource>); otherwise, the client SHOULD ignore
    ;; the "roster push".
    (unless (or (null from) 
		(jid-equal (jid-user from) (jid-user (client-actual-jid c))))
      ;; Roster from untrusted source
      (warn "Untrusted roster push from ~S ignored." from)
      (return-from handle-roster)))

  (when (string= (stanza-type i) "error")
    (return-from handle-roster))
  (when (string= (stanza-type i) "result")
    (setf (got-roster c) t))

  (let* ((query (find "query" (jabber:stanza-children i) :key #'xmls:node-name
		      :test #'string=))
	 (items (remove-if 
		 #'(lambda (child)
		     (or (string/= (xmls:node-name child) "item")
			 (string/= (xmls:node-ns child) +roster-namespace+)))
		 (xmls:node-children query))))
    (dolist (item items)
      (let ((jid (xmls:get-attr item "jid"))
	    (name (xmls:get-attr item "name"))
	    (subscription (xmls:get-attr item "subscription"))
	    (ask (xmls:get-attr item "ask"))
	    (groups (loop for child in (xmls:node-children item)
		       when (string= (xmls:node-name child) "group")
		       collect (car (xmls:node-children child)))))
	(cond
	  ((string= subscription "remove")
	   (remhash (jid-normalize jid) (roster c)))
	  (t
	   (setf (gethash (jid-normalize jid) (roster c))
		 (list :name name
		       :subscription
		       (or (cdr (assoc subscription
				       '(("to" . :to)
					 ("from" . :from)
					 ("both" . :both)
					 ("none" . :none))
				       :test #'string=))
			   :none)
		       :ask (when (string= ask "subscribe") :subscribe)
		       :groups groups))))))))

(defmethod get-roster-plist ((c roster-handling-client) jid)
  "Return plist of roster item JID.
Return nil if JID is not in roster."
  (gethash (jid-normalize jid) (roster c)))

(defmethod change-roster-plist ((c roster-handling-client) jid plist &key callback)
  "Ask the server to change roster entry for JID.
Asking for subscription changes this way doesn't make sense."
  (send-iq c
	   `(("query" . ,jabber:+roster-namespace+)
	     ()
	     ("item" (("jid" ,jid)
		      ,@(when (getf plist :name)
			      (list `("name" ,(getf plist :name)))))))
	   :type "set"
	   :callback callback))

(defmethod remove-roster-item ((c roster-handling-client) jid &key callback)
  "Ask the server to remove JID from roster."
  (send-iq c
	   `(("query" . ,jabber:+roster-namespace+)
	     ()
	     ("item" (("jid" ,jid)
		      ("subscription" "remove"))))
	   :type "set"
	   :callback callback))

(defmethod get-roster-name ((c roster-handling-client) jid)
  "Return roster name for JID.
Return nil if JID is not in roster, or has no assigned name."
  (getf (gethash (jid-normalize jid) (roster c)) :name))

(defmethod have-subscription-to ((c roster-handling-client) jid)
  "Return true if this client has presence subscription to JID.
That is, subscription type is \"to\" or \"both\"."
  (member (getf (gethash (jid-normalize jid) (roster c)) :subscription)
	  '(:to :both)))

(defmethod have-subscription-from ((c roster-handling-client) jid)
  "Return true if JID has presence subscription to this client.
That is, subscription type is \"from\" or \"both\"."
  (member (getf (gethash (jid-normalize jid) (roster c)) :subscription)
	  '(:from :both)))

;; arch-tag: 18b7e7d0-ca67-11da-9977-000a95c2fcd0

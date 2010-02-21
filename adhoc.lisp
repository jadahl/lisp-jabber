(in-package :jabber)

;;; Helpers for JEP-0050 implementations (serverside)

(defparameter +adhoc-namespace+ "http://jabber.org/protocol/commands"
  "The namespace string for ad-hoc commands (JEP-0050).")

(defclass adhoc-handling-connection (disco-handling-connection)
  ((commands :accessor adhoc-commands
	     :initform (make-hash-table :test #'equal)
	     :documentation "Hash table for ad-hoc commands.
The keys are cons cells whose cars are strings containing the JID 
to listen for (or nil for any JID) and whose strings containing
node names.  The values are lists of the form (ACCESS-PREDICATE
FUNCTION NAME).

ACCESS-PREDICATE is either t, which means to always grant access,
or a function taking the connection, the JID of the requester and
the JID of the target as arguments, and returning non-nil to
grant access.

FUNCTION is a function to be called when the command is called.
It is called with arguments CONNECTION, FROM, TO, NODE, ACTION,
SESSION-ID, COMMAND, and CLOSURE-DATA.  It returns the following
values:
 - STATUS, a string to return as the `status' attribute of the
   <command/> node (one of \"executing\", \"completed\" and
   \"canceled\").
 - ACTIONS, a list of strings enumerating the allowed actions.
   The first entry is the default action.  Possible actions are
   \"execute\", \"next\", \"prev\", \"complete\" and \"cancel\".
 - XML-NODES, a list of extra XML nodes to add to the response.
 - CLOSURE-DATA, a piece of data to store as closure data for
   this session (or nil, to erase stored data).

Both ACCESS-PREDICATE and FUNCTION may signal an error of type
jabber-error, which will be returned to the sender.

NAME is a human-readable name.")
   (adhoc-session-sender :accessor adhoc-session-sender
			 :initform (make-hash-table :test #'equal)
			 :documentation "Hash table for sender JIDs.
The keys are strings, namely session ids.  The values are JIDs,
the only one allowed to access the session.  This is to prevent
other entities from hijacking an AHC session.")
   (adhoc-closure :accessor adhoc-closure
		  :initform (make-hash-table :test #'equal)
		  :documentation "Hash table for closure data.
The keys are strings, namely session ids.  The values are
arbitrary data.")
   (adhoc-jids :accessor adhoc-jids
	       :initform ()
	       :documentation "List of handled JIDs, or t if all JIDs handled.")))

(defmethod authenticated :after ((c adhoc-handling-connection))
  (add-adhoc-handlers c))

(defmethod add-adhoc-handlers ((c adhoc-handling-connection))
  (let ((set-table (connection-iq-set c)))
    (setf (gethash +adhoc-namespace+ set-table) #'handle-adhoc)))

(defmethod add-adhoc-command ((c adhoc-handling-connection) jid node function access-predicate name)
  "Start handling a new adhoc command.
JID is the JID to listen for, or nil for any JID.
NODE is the node name of the command."
  (if (null jid)
      (setf (adhoc-jids c) t)
    (unless (eq (adhoc-jids c) t)
      (pushnew jid (adhoc-jids c) :test #'string-equal)))
  (setf (gethash (cons jid node) (adhoc-commands c)) (list access-predicate function name)))

(defmethod handle-adhoc ((c adhoc-handling-connection) (i iq))
  (let* ((from (normal-from i))
	 (to (normal-to i))
	 (command (find "command" (stanza-children i)
			:key #'xmls:node-name :test #'string=))
	 (node (second (assoc "node" (xmls:node-attrs command) :test #'string=)))
	 (entry (or (gethash (cons to node) (adhoc-commands c))
		    (gethash (cons nil node) (adhoc-commands c))))
	 (action (or (second (assoc "action" (xmls:node-attrs command) :test #'string=))
		     "execute"))	;"execute" is the implied action
	 (sessionid (second (assoc "sessionid" (xmls:node-attrs command) :test #'string=)))
	 (valid-sender (if (null sessionid)
			   t
			 (string-equal (gethash sessionid (adhoc-session-sender c)) from)))
	 (closure-data (gethash sessionid (adhoc-closure c))))
    (handler-case
      (progn
	;; Basic validation
	(when (null entry)
	  (error 'jabber-error :type "cancel" :condition "item-not-found"))
	(unless valid-sender
	  (error 'jabber-error :type "modify" :condition "bad-request"
		 :app-specific `(("bad-sessionid" . ,+adhoc-namespace+))))
	(unless (member action '("cancel" "complete" "execute" "next" "prev") :test #'string=)
	  (error 'jabber-error :type "modify" :condition "bad-request"
		 :app-specific `(("malformed-action" . ,+adhoc-namespace+))))

	(destructuring-bind (access-predicate function name) entry
	  (declare (ignore name))
	  ;; Access control
	  (if (not (or (eq access-predicate t)
		       (funcall access-predicate c from to)))
	      (error 'jabber-error :type "cancel" :condition "forbidden")
	    ;; Call the function, offer restarts for errors
	    (multiple-value-bind (status actions xml-nodes new-closure-data)
		(restart-case
		 (funcall function c from to node (intern (string-upcase action) "KEYWORD") sessionid command closure-data)
		 (return-adhoc-error
		  () :report "Return an error to the sender, by JEP-0050."
		  (values "completed" nil '(("note" (("type" "error")) "Internal error")) nil))
		 (return-iq-error
		  () :report "Return an IQ error to the sender."
		  (error 'jabber-error :type "wait" :condition "internal-server-error")))

	      ;; Now, we've called the function.  Construct the response.
	      ;; Generate a random session id if we don't have one already.
	      (let ((new-session-id (or sessionid (princ-to-string (random 1000000)))))
		(setf (gethash new-session-id (adhoc-session-sender c)) from)
		(if new-closure-data
		    (setf (gethash new-session-id (adhoc-closure c)) new-closure-data)
		  (remhash new-session-id (adhoc-closure c)))
		(send-iq-result
		 c i
		 :payload `(("command" . ,+adhoc-namespace+)
			    (("sessionid" ,new-session-id)
			     ("node" ,node)
			     ("status" ,status))
			    ,@(when actions
				(list `("actions"
					(("execute" ,(car actions)))
					,@(mapcar #'list actions))))
			    ,@xml-nodes)))))))
      (jabber-error 
       (e)
       (send-stanza-error 
	c i
	:type (jabber-error-type e) :condition (jabber-error-condition e)
	:text (text e) :app-specific (app-specific e))))))

(defmethod get-disco-info append ((c adhoc-handling-connection) from to node)
  (declare (ignore from))
  (let ((handle-this-jid (or 
			  ;; If we handle all JIDs...
			  (eq (adhoc-jids c) t)
			  ;; ...or this specific JID...
			  (member to (adhoc-jids c) :test #'string-equal))))
    (cond
     ((eq node nil)
      ;; The entity itself is queried...
      (when handle-this-jid
	;; ...return the adhoc feature on queries.
	`((:feature . ,+adhoc-namespace+))))
     ;; The command list is queried...
     ((string= node +adhoc-namespace+)
      (when handle-this-jid
	'((:identity :category "automation" :type "command-list"))))
     (t
      ;; A specific node is queried...
      (let ((the-command (or (gethash (cons to node) (adhoc-commands c))
			     (gethash (cons nil node) (adhoc-commands c)))))
	(when the-command
	  `((:identity :category "automation" :type "command" :name ,(third the-command))
	    (:feature . ,+adhoc-namespace+)
	    (:feature . "jabber:x:data"))))))))

(defmethod get-disco-items append ((c adhoc-handling-connection) from to disco-node)
  (when (string= disco-node +adhoc-namespace+)
    (let (commands)
      (maphash #'(lambda (key value)
		   (destructuring-bind (jid . node) key
		     (destructuring-bind (access-predicate function name) value
		       (declare (ignore function))
		       ;; If the JID matches (either specifically or by wildcard)...
		       (if (and (or (eq jid nil)
				    (string-equal jid to))
				;; ...and access control passes...
				(or (eq access-predicate t)
				    (ignore-errors
				      (funcall access-predicate c from to))))
			   ;; ...return it.
			   (push (list :jid to :node node :name name) commands)))))
	       (adhoc-commands c))
      commands)))

;; arch-tag: 7bbfaa6e-ec70-11d9-883c-000a95c2fcd0

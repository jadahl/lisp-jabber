(in-package :jabber)

;;; You might want to subclass disco-handling-connection, or
;;; just use the helper functions make-disco-items and
;;; make-disco-info.

(defparameter +disco-info-namespace+ "http://jabber.org/protocol/disco#info"
  "The namespace string for disco info requests.")

(defparameter +disco-items-namespace+ "http://jabber.org/protocol/disco#items"
  "The namespace string for disco items requests.")

(defclass disco-handling-connection (connection)
  ((info :accessor disco-info
	 :initform (make-hash-table :test #'equal)
	 :documentation "Hash table for data about disco nodes.
The keys are cons cells, the car being a string naming the full
JID for of this entry, and the cdr being a string naming a node,
or nil, for requests without a node.  The values are lists of the
form (IDENTITIES FEATURES ACCESS-PREDICATE).

IDENTITIES is a list of property lists with keys :CATEGORY,
:TYPE, and :NAME, where :NAME is optional.

FEATURES is a list of strings, each of which is a namespace.

ACCESS-PREDICATE is either t, which means to always grant access,
or a function taking the JID of the requester as argument, and
returning non-nil to grant access.")
   (items :accessor disco-items
	  :initform (make-hash-table :test #'equal)
	  :documentation "Hash table for data about disco nodes.
The keys are cons cells, the car being a string naming the full
JID for of this entry, and the cdr being a string naming a node,
or nil, for requests without a node.  The values are lists of
the form (ITEMS ACCESS-PREDICATE).

ITEMS is a list of property lists with keys :JID, :NAME,
and :NODE, where only :JID is required.

ACCESS-PREDICATE is either t, which means to always grant access,
or a function taking the JID of the requester as argument, and
returning non-nil to grant access."))
   (:documentation
   "This is a base class handling disco requests automatically.
There are two ways to use it: insert data into the hash table
using disco-{items,info}, or specialize the get-disco-{items,info}
methods, fetching information as needed."))

(defmethod add-disco-info ((c disco-handling-connection) jid node identities features)
  "Convenience function for adding disco information to a jid/node
combination.
If there is already some disco information for this node, the given
data is merged with the existing data, and duplicates are suppressed.
If there is no data for this node, the data is entered, with access
allowed to all."
  (multiple-value-bind (entry found-p) (gethash (cons jid node) (disco-info c))
    (if found-p
	(progn
	  (setf (first entry) (union (first entry) identities :test #'equal))
	  (setf (second entry) (union (second entry) features :test #'equal)))
      (setf (gethash (cons jid node) (disco-info c))
	    (list identities features t)))))

(defmethod add-disco-items ((c disco-handling-connection) jid node items)
  "Convenience function for adding disco items to a jid/node
combination.
If there are already some disco items for this node, the given
data is merged with the existing data, and duplicates are suppressed.
If there is no data for this node, the data is entered, with access
allowed to all."
  (multiple-value-bind (entry found-p) (gethash (cons jid node) (disco-items c))
    (if found-p
	(setf (first entry) (union (first entry) items :test #'equal))
      (setf (gethash (cons jid node) (disco-info c))
	    (list items t)))))

(defmethod authenticated :after ((c disco-handling-connection))
  (add-disco-handlers c))

(defmethod add-disco-handlers ((c disco-handling-connection))
  (let ((get-table (connection-iq-get c)))
    (setf (gethash +disco-info-namespace+ get-table) #'handle-disco-info-get)
    (setf (gethash +disco-items-namespace+ get-table) #'handle-disco-items-get)))

(defmethod handle-disco-info-get ((c disco-handling-connection) (i iq))
  (let* ((from (normal-from i))
	 (jid (normal-to i))
	 (query (find "query" (stanza-children i)
		      :key #'xmls:node-name :test #'string=))
	 (node (second (assoc "node" (xmls:node-attrs query) :test #'string=))))
    (handler-case
     (let ((info (get-disco-info c from jid node))
	   identities features)
       (if info
	   (progn
	     (dolist (piece info)
	       (when (consp piece)
		 (cond
		  ((eq (car piece) :identity)
		   (push (cdr piece) identities))
		  ((eq (car piece) :feature)
		   (push (cdr piece) features)))))
	     (when (null identities)
	       (warn "Returning disco#info result with zero identities."))
	     (when (null features)
	       (warn "Returning disco#info result with zero features."))
	     (send-iq-result c i
			     :payload
			     (make-disco-info identities 
					      (remove-duplicates features :test #'string=)
					      node)))
	 (error 'jabber-error :type "cancel" :condition "item-not-found")))
     (jabber-error (e)
		   (send-stanza-error c i
				      :type (jabber-error-type e)
				      :condition (jabber-error-condition e)
				      :text (text e)
				      :app-specific (app-specific e))))))

(defmethod handle-disco-items-get ((c disco-handling-connection) (i iq))
  (let* ((from (normal-from i))
	 (jid (normal-to i))
	 (query (find "query" (stanza-children i)
		      :key #'xmls:node-name :test #'string=))
	 (node (second (assoc "node" (xmls:node-attrs query) :test #'string=))))
    (handler-case
	;; Get items from methods.
	(let ((items (get-disco-items c from jid node)))
	  ;; Got any?
	  (if items
	      ;; Yes, return them.
	      (send-iq-result c i
			      :payload
			      (make-disco-items (remove-if-not #'listp items) node))
	      ;; No.  Was the request to a specific node?
	      (if node
		  ;; If so, return "node not found".
		  (error 'jabber-error :type "cancel" :condition "item-not-found")
		  ;; Otherwise, return an empty result, meaning "no items".
		  (send-iq-result c i
				  :payload
				  (make-disco-items '() node)))))
      (jabber-error (e)
	(send-stanza-error c i
			   :type (jabber-error-type e)
			   :condition (jabber-error-condition e)
			   :text (text e)
			   :app-specific (app-specific e))))))

(defgeneric get-disco-info (c from to node)
  (:method-combination append)
  (:documentation "Return disco info.
FROM is JID requesting info.  TO is JID that info is being requested
about.  NODE is the node concerned by the request.
This function returns a list, whose elements are either of these:
 - T
 - (:IDENTITY . IDENTITY)
 - (:FEATURE . FEATURE)
The syntax of IDENTITY and FEATURE are described elsewhere.
If the list is empty, the given JID/node combination does not exist,
and the requester should be informed about that.
If the list contains only T elements, the given JID/node combination
does exist, but has neither identities nor features.
Otherwise, the identities and features present in the list are the
ones to report.
This function may signal an error of type JABBER-ERROR if it has an
opinion of what error message should be sent.

As a consequence of the above, a method must not return an empty
list if it knows that the JID/node combination exists.  Rather,
return (T) if you have nothing more interesting to return.")
  (:method
   append ((c disco-handling-connection) from to node)
   (let ((entry (gethash (cons to node) (disco-info c))))
     (if entry
	 (destructuring-bind (identities features access-predicate) entry
	   (if (or (eq access-predicate t)
		   (funcall access-predicate from))
	       (progn
		 ;; By JEP-0030, an entity must return at least one feature,
		 ;; the disco#info feature.  As there is no easy way to
		 ;; determine that this entity does NOT support disco#items,
		 ;; we include that as well.
		 (pushnew +disco-info-namespace+ features :test #'string=)
		 (pushnew +disco-items-namespace+ features :test #'string=)
		 (append (mapcar #'(lambda (identity) 
				     (cons :identity identity))
				 identities)
			 (mapcar #'(lambda (feature)
				     (cons :feature feature))
				 features)))
	     (error 'jabber-error
		    :type "auth"
		    :condition "forbidden")))))))

(defgeneric get-disco-items (c from to node)
  (:method-combination append)
  (:documentation "Return disco items.
FROM is JID requesting info.  TO is JID that info is being requested
about.  NODE is the node concerned by the request.
This function returns a list, whose elements are either of these:
 - T
 - ITEM
The syntax of ITEM is described elsewhere.
If the list is empty, the given JID/node combination does not exist,
and the requester should be informed about that.
If the list contains only T elements, the given JID/node combination
does exist, but has neither identities nor features.
Otherwise, the items present in the list are the ones to report.
This function may signal an error of type JABBER-ERROR if it has an
opinion of what error message should be sent.

As a consequence of the above, a method must not return an empty
list if it knows that the JID/node combination exists.  Rather,
return (T) if you have nothing more interesting to return.")
  (:method
   append ((c disco-handling-connection) from to node)
   (let ((entry (gethash (cons to node) (disco-items c))))
     (if entry
	 (destructuring-bind (items access-predicate) entry
	   (if (or (eq access-predicate t)
		   (funcall access-predicate from))
	       (or items (list t))
	     (error 'jabber-error
		    :type "auth"
		    :condition "forbidden")))))))

(defun make-disco-info (identities features &optional node)
  "Create a disco#info reply from IDENTITIES and FEATURES.
IDENTITIES is a list of property lists with keys :CATEGORY,
:TYPE, and :NAME, where :NAME is optional.
FEATURES is a list of strings, each of which is a namespace.
NODE is the name of the queried node, if any.

The node returned is a <query/> node meant to be wrapped
inside an <iq/> stanza of type \"result\"."
  `(("query" . ,+disco-info-namespace+)
    (,@(when node
	 `(("node" ,node))))
    ,@(mapcar
       #'(lambda (identity)
	   `("identity"
	     (("category" ,(getf identity :category))
	      ("type" ,(getf identity :type))
	      ,@(when (getf identity :name)
		  `(("name" ,(getf identity :name)))))))
       identities)
    ,@(mapcar
       #'(lambda (feature)
	   `("feature" (("var" ,feature))))
       features)))

(defun parse-disco-info (query)
  "Parse a disco#info reply and return the information.
Two values are returned, IDENTITIES and FEATURES.
IDENTITIES is a list of property lists with keys :CATEGORY,
:TYPE, and :NAME, where :NAME is optional.
FEATURES is a list of strings, each of which is a namespace."
  (flet ((get-attr 
	  (attr child)
	  (let* ((attrs (xmls:node-attrs child))
		 (the-attr (assoc attr attrs :test 'string=)))
	    (when the-attr
	      (list (intern (string-upcase attr) "KEYWORD")
		    (second the-attr))))))
    (let (identities features)
      (dolist (child (xmls:node-children query))
	(cond
	 ((string= (xmls:node-name child) "identity")
	  (push (append (get-attr "category" child)
			(get-attr "type" child)
			(get-attr "name" child))
		identities))

	 ((string= (xmls:node-name child) "feature")
	  (push (second (assoc "var" (xmls:node-attrs child) :test 'string=))
		features))))
    
      (values identities features))))

(defun make-disco-items (items &optional node)
  "Create a disco#items reply from ITEMS.
ITEMS is a list of property lists with keys :JID, :NAME,
and :NODE, where only :JID is required.
NODE is the name of the queried node, if any.

The node returned is a <query/> node meant to be wrapped
inside an <iq/> stanza of type \"result\"."
  `(("query" . ,+disco-items-namespace+)
    (,@(when node
	 `(("node" ,node))))
    ,@(mapcar
       #'(lambda (item)
	   (let ((jid (getf item :jid))
		 (name (getf item :name))
		 (node (getf item :node)))
	     `("item"
	       (("jid" ,jid)
		,@(when name `(("name" ,name)))
		,@(when node `(("node" ,node)))))))
       items)))

(defun parse-disco-items (query)
  "Parse a disco#items reply and return the information.
Return a list of property lists with keys :JID, :NAME and :NODE."
  (flet ((get-attr 
	  (attr child)
	  (let* ((attrs (xmls:node-attrs child))
		 (the-attr (assoc attr attrs :test 'string=)))
	    (when the-attr
	      (list (intern (string-upcase attr) "KEYWORD")
		    (second the-attr))))))
    (mapcar #'(lambda (child)
		(append (get-attr "jid" child)
			(get-attr "name" child)
			(get-attr "node" child)))
	    (remove-if-not #'(lambda (child)
				(string= (xmls:node-name child) "item"))
			   (xmls:node-children query)))))

;; arch-tag: 4dd63eb2-d391-11d9-aed2-000a95c2fcd0

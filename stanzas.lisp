(in-package :jabber)

(defclass stanza ()
  ((name :initarg :name
	 :reader stanza-name)
   (from :initarg :from
	 :reader stanza-from)
   (to :initarg :to
       :reader stanza-to)
   (type :initarg :type
	 :reader stanza-type)
   (id :initarg :id
       :reader stanza-id)
   (node :initarg :node
	 :reader stanza-node)

   normal-from normal-to))

(defclass message (stanza) ())
(defclass iq (stanza) ())
(defclass presence (stanza) ())

(defun parse-stanza (xml-data)
  "Return a stanza instance based upon XML-DATA."
   (let ((name (xmls:node-name xml-data))
	 (from (second (assoc "from" (xmls:node-attrs xml-data) :test #'string=)))
	 (to (second (assoc "to" (xmls:node-attrs xml-data) :test #'string=)))
	 (type (second (assoc "type" (xmls:node-attrs xml-data) :test #'string=)))
	 (id (second (assoc "id" (xmls:node-attrs xml-data) :test #'string=))))
     (make-instance (cond
		     ((equal name "message")
		      'message)
		     ((equal name "iq")
		      'iq)
		     ((equal name "presence")
		      'presence)
		     (t
		      (error 'unexpected-stanza-error :stanza xml-data)))
		    :name name
		    :from from
		    :to to
		    :type type
		    :id id
		    :node xml-data)))

(defmethod stanza-children ((stanza stanza))
  (xmls:node-children (stanza-node stanza)))

(declaim (inline normal-from))
(defun normal-from (stanza)
  "Return the normalized 'from' attribute of STANZA.
Memoizes the result."
  (if (slot-boundp stanza 'normal-from)
      (slot-value stanza 'normal-from)
    (setf (slot-value stanza 'normal-from)
	  (jid-normalize (stanza-from stanza)))))

(declaim (inline normal-to))
(defun normal-to (stanza)
  "Return the normalized 'to' attribute of STANZA.
Memoizes the result."
  (if (slot-boundp stanza 'normal-to)
      (slot-value stanza 'normal-to)
    (setf (slot-value stanza 'normal-to)
	  (jid-normalize (stanza-to stanza)))))

(defmethod message-body ((m message))
  "Return the contents of the body tag of M, or nil if none."
  (let ((body-tag (assoc "body" (stanza-children m) :test #'string-equal)))
    (when body-tag
      (first (xmls:node-children body-tag)))))

(defmethod iq-xmlns ((i iq))
  "Return the namespace of an iq stanza."
  (dolist (c (stanza-children i))
    (let ((name (xmls:node-name c))
	  (ns (xmls:node-ns c)))
      (when (not (string= name "error"))
	(return ns)))))

(defmethod find-stanza-error ((s stanza))
  (find "error" (stanza-children s)
	:key 'xmls:node-name :test 'string=))

(defun stanza-error-condition (stanza-error)
  "Find the condition element of a stanza error."
  (xmls:node-name (find-if
		   (lambda (node)
		     (and (string= (xmls:node-ns node) "urn:ietf:params:xml:ns:xmpp-stanzas")
			  (string/= (xmls:node-name node) "text")))
		   (xmls:node-children stanza-error))))

(defun stanza-error-type (stanza-error)
  "Return the type of the stanza error."
  (second (assoc "type" (xmls:node-attrs stanza-error) :test #'string=)))

(defun stanza-error-text (stanza-error)
  "Return the text of a stanza error."
  (let ((text-node (find '("text" . "urn:ietf:params:xml:ns:xmpp-stanzas")
			 (xmls:node-children stanza-error)
			 :test 'equal)))
    (when text-node
      (car (xmls:node-children text-node)))))

(defun stanza-error-app-specific (stanza-error)
  "Return app-specific data in stanza error, or nil if none."
  (find-if
   (lambda (node) (string/= (xmls:node-ns node) "urn:ietf:params:xml:ns:xmpp-stanzas"))
   (xmls:node-children stanza-error)))

(defun signal-stanza-error (condition-type stanza-error)
  (error condition-type
	 :condition (stanza-error-condition stanza-error)
	 :type (stanza-error-type stanza-error)
	 :text (stanza-error-text stanza-error)
	 :app-specific (stanza-error-app-specific stanza-error)))

;;; arch-tag: bdfda8fa-456c-4b05-b3db-26bea5548e35

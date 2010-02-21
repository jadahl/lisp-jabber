(in-package :jabber)

;;; Helper functions for JEP-0077 implementations.  JEP-0077 has five
;;; steps:
;;;
;;; 1. Client requests form. (empty <query> element with proper namespace)
;;; 2. Server returns form.
;;; 3. Client fills out form and sends it.
;;; 4. Server parses form.
;;; 5. Server reports success (empty <iq> result) or failure (ordinary
;;;    iq error).
;;;
;;; Currently, there are helpers for steps 2 and 4.

(defun make-register-get-form (fields registered-p instructions
				      &optional xdata-form)
  "Create a jabber:iq:register reply from the given data.
FIELDS is an alist where the keys are strings naming the
requested fields.  See JEP-0077 for the list of allowed fields.  The
values are either strings, for current values, or nil.
REGISTERED-P indicates whether the user is already registered.
INSTRUCTIONS is a string containing instructions for the user.
XDATA-FORM is either nil, or the XML structure of a jabber:x:data form
\(possibly created by OUTPUT-XDATA-FORM).

The node returned is a <query/> node meant to be wrapped
inside an <iq/> stanza of type \"result\".

If you only support x:data forms, set FIELDS to nil, and tell the user
that he is using an obsolete client in INSTRUCTIONS."
  `(("query" . "jabber:iq:register")
    ()
    ,@(when registered-p
	'(("registered")))
    ,@(mapcar
       #'(lambda (field)
	   `(,(car field)
	     ()
	     ,(cdr field)))
       fields)
    ,@(when instructions
	`(("instructions" () ,instructions)))
    ,@(when xdata-form
	(list xdata-form))))

(defun read-register-set-form (xml-data)
  "Parse the given filled-out registration form.
Return multiple values:
- FIELDS, an alist mapping field names to field values
- REMOVE-P, non-nil if the user wants to unregister
- XDATA-FORM, the raw XML data of the x:data form, if any

XML-DATA is a <query> element in the jabber:iq:register
namespace."
  (let ((children (xmls:node-children xml-data))
	fields remove-p xdata-form)
    (setf remove-p (find "remove" children
			 :key #'xmls:node-name
			 :test #'string=))
    (when remove-p 
      (setf children (remove remove-p children)))
    (setf xdata-form (find '("x" . "jabber:x:data") children
			   :key #'car
			   :test #'equal))
    (when xdata-form
      (setf children (remove xdata-form children)))
    ;; Remaining children must be fields.
    (setf fields
	  (mapcar
	   #'(lambda (element)
	       (cons (xmls:node-name element)
		     ;; assume that there is one child, and that it is
		     ;; a string.
		     (car (xmls:node-children element))))
	   children))
    (values fields remove-p xdata-form)))

;; arch-tag: e197c824-ea56-11d9-88a9-000a95c2fcd0

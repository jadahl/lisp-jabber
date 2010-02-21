(in-package :jabber)

(defclass xform ()
  ((type :accessor xform-type
	 :type (member :form :submit :cancel :result)
	 :initarg :type)
   (title :accessor xform-title
	  :type string
	  :initarg :title)
   (instructions :accessor xform-instructions
		 :type string
		 :initarg :instructions)
   ;; for form and submit, the obvious data.
   ;; for result, fields in <reported/> if there is
   ;; such a tag, otherwise the obvious data.
   (fields :accessor xform-fields
	   :initarg :fields)
   ;; for form, subimt and cancel, always nil.
   ;; for result, if multiple items, lists of field lists.
   (items :accessor xform-items
	  :initarg :items)))

(defclass xform-field ()
  ((desc :accessor xform-field-desc
	 :type string
	 :initarg :desc)
   (label :accessor xform-field-label
	  :type string
	  :initarg :label)
   (var :accessor xform-field-var
	:type string
	:initarg :var)
   (required :accessor xform-field-required
	     :type (member t nil)
	     :initform nil
	     :initarg :required)
   (values :accessor xform-field-values
	   :initform ()
	   :initarg :values)
   (options :accessor xform-field-options
	    :type (list string)
	    :initform ()
	    :initarg :options)
   (type :accessor xform-field-type
	 :type (or (member :boolean :fixed :hidden
			   :jid-multi :jid-single
			   :list-multi :list-single
			   :text-multi :text-single
			   :text-private)
		   string)
	 :initarg :type)))

(defun parse-xdata-form (xml-data)
  "Return a xform instance based upon XML-DATA.
XML-DATA should be an <x/> tag in the jabber:x:data namespace."
  (let ((type (second (assoc "type" (xmls:node-attrs xml-data) :test #'string=)))
	(reported (find "reported" (xmls:node-children xml-data) :test #'string= :key 'xmls:node-name))
	(items (remove-if (lambda (node-name) (string/= node-name "item"))
			  (xmls:node-children xml-data) :key 'xmls:node-name))
	(fields (remove-if (lambda (node-name) (string/= node-name "field"))
			   (xmls:node-children xml-data) :key 'xmls:node-name)))
    (make-instance 'xform :type (cdr (assoc type
					    '(("form" . :form)
					      ("submit" . :submit)
					      ("cancel" . :cancel)
					      ("result" . :result))
					    :test #'string=))
		   :fields (mapcar 'parse-xdata-field (if reported
							  (xmls:node-children reported)
							fields))
		   :items (mapcar 'parse-xdata-item items))))

(defun parse-xdata-field (field)
  (let ((type (second (assoc "type" (xmls:node-attrs field) :test #'string=)))
	(var (second (assoc "var" (xmls:node-attrs field) :test #'string=)))
	(label (second (assoc "label" (xmls:node-attrs field) :test #'string=)))
	(required (if (find "required" (xmls:node-children field) :key 'xmls:node-name)
		      t
		    nil))
	(values (remove-if (lambda (node-name) (string/= node-name "value"))
			   (xmls:node-children field) :key 'xmls:node-name))
	(options (remove-if (lambda (node-name) (string/= node-name "option"))
			    (xmls:node-children field) :key 'xmls:node-name))
	(desc (find "desc" (xmls:node-children field) :key 'xmls:node-name)))
    (make-instance 'xform-field :type (cdr (assoc type
						  '(("boolean" . :boolean)
						    ("fixed" . :fixed)
						    ("hidden" . :hidden)
						    ("jid-multi" . :jid-multi)
						    ("jid-single" . :jid-single)
						    ("list-multi" . :list-multi)
						    ("list-single" . :list-single)
						    ("text-multi" . :text-multi)
						    ("text-single" . :text-single)
						    ("text-private" . :text-private))
						  :test 'string=))
		   :var var
		   :desc (car (xmls:node-children desc))
		   :label label
		   :required required
		   :values (mapcar (lambda (v) (car (xmls:node-children v))) values)
		   :options (mapcar 'parse-xdata-option options))))

(defun parse-xdata-option (option)
  (let ((label (second (assoc "label" (xmls:node-attrs option) :test 'string=)))
	(value (car (xmls:node-children (find "value" (xmls:node-children option)
					      :key 'xmls:node-name)))))
    (cons value label)))

(defmethod output-xdata-form ((form xform))
  (let (children)
    (when (slot-boundp form 'title)
      (push (xmls:make-node :name "title"
			    :child (xform-title form))
	    children))
    (when (slot-boundp form 'instructions)
      (push (xmls:make-node :name "instructions"
			    :child (xform-instructions form))
	    children))
    (if (and (slot-boundp form 'items) (xform-items form))
	;; if we have items, we have reported fields
	(progn
	  (push (xmls:make-node :name "reported"
				:children (mapcar 'output-xdata-field (xform-fields form)))
		children)

	  ;; take care to add items in the given order
	  (nconc children (mapcar #'(lambda (item-fields)
				      (xmls:make-node
				       :name "item"
				       :children (mapcar 'output-xdata-field item-fields)))
				  (xform-items form))))
      ;; else, we have only normal fields
      (setf children
	    (nconc children (mapcar 'output-xdata-field (xform-fields form)))))
    
    (xmls:make-node :name "x" :ns "jabber:x:data"
		    :attrs (list (list "type" (cdr (assoc (xform-type form)
							  '((:form . "form")
							    (:submit . "submit")
							    (:cancel . "cancel")
							    (:result . "result"))))))
		    :children children)))

(defmethod output-xdata-field ((field xform-field))
  (let (children attributes)
    (when (and (slot-boundp field 'desc) (xform-field-desc field))
      (push (xmls:make-node :name "desc" :child (xform-field-desc field))
	    children))
    (when (and (slot-boundp field 'label) (xform-field-label field))
      (push (list "label" (xform-field-label field))
	    attributes))
    (when (and (slot-boundp field 'var) (xform-field-var field))
      (push (list "var" (xform-field-var field))
	    attributes))
    (when (xform-field-required field)
      (push (xmls:make-node :name "required") children))
    (dolist (value (xform-field-values field))
      (push (xmls:make-node :name "value" :child value)
	    children))
    (dolist (option (xform-field-options field))
      (push (xmls:make-node :name "option" 
			    :attrs (when (cdr option)
				     (list (list "label" (cdr option))))
			    :child
			    (xmls:make-node :name "value"
					    :child (car option)))
	    children))
    (when (and (slot-boundp field 'type) (xform-field-type field))
      (push (list "type" (string-downcase (symbol-name (xform-field-type field))))
	    attributes))
    (xmls:make-node :name "field" :attrs attributes :children children)))

(defmethod find-field ((form xform) var)
  "Find field from form with name VAR."
  (find var (xform-fields form) :key 'jabber::xform-field-var :test 'string=))

;; arch-tag: f8563bdf-bb13-4eeb-94b5-4e3aa10ca7d5
   
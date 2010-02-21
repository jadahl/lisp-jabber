;;; This is an example of writing a component that handles ad-hoc
;;; commands (JEP-0050).
;;;
;;; To run it, call the do-test-adhoc function with appropriate
;;; arguments.  The defaults are correct for an out-of-the-box
;;; jabberd2 server running on localhost.
;;;
;;; Right now (August 2005), there are very few clients supporting
;;; ad-hoc commands.  jabber.el does, and there are patches for Psi as
;;; well.

(in-package :cl-user)

(defclass test-adhoc (jabber:adhoc-handling-connection jabber:component)
  ())

(defun do-test-adhoc (&key (jid "adhoc.localhost") (router-host "localhost") (router-port 5347) (secret "secret"))
  (let ((con (make-instance 'test-adhoc :jid jid :router-host router-host :router-port router-port :secret secret)))
    (jabber:add-disco-info con jid nil '((:category "component" :type "x-test" :name "Ad-hoc commands test")) ())
    
    (jabber:add-adhoc-command con nil "foo" 'the-command t "Do nothing")
    (jabber:add-adhoc-command con nil "wrong" 'the-wrong-command t "Perform a division by zero")
    (jabber:add-adhoc-command con nil "add" 'add-command t "Interactive addition")
    
    (jabber:connect con)
    (unwind-protect
	(loop (jabber:read-and-act con :block t))
      (jabber:connection-close con))))

(defmethod the-command ((c test-adhoc) from to node action session-id command closure-data)
  (values "completed" nil nil nil))

(defmethod the-wrong-command ((c test-adhoc) from to node action session-id command closure-data)
  (/ 1 0)
  (values "completed" nil nil nil))

(defmethod add-command ((c test-adhoc) from to node action session-id command closure-data)
  (flet ((make-form 
	  ()
	  (let ((form (make-instance 'jabber:xform :type :form :title "Add numbers"
				     :instructions "Enter a number, to add it to the list of numbers to add.
Hit 'Complete' to really add the numbers.  'Previous' and 'Cancel' should work as expected."
				     :fields
				     (list
				      (make-instance 'jabber:xform-field
						     :type :fixed
						     :values (list (format nil "So far, there are ~A numbers in the list." (length closure-data))))
				      (make-instance 'jabber:xform-field
						     :var "number"
						     :label "Enter a number"
						     :desc "This is where you type a number to add."
						     :required t
						     :type :text-single)))))
	    (jabber:output-xdata-form form))))
    (cond
     ((member action '(:execute :next))
      (let* ((xdata-form (find '("x" . "jabber:x:data") (xmls:node-children command) :test #'equal :key #'car))
	     (parsed-form (when xdata-form (jabber:parse-xdata-form xdata-form))))
	(when (and parsed-form (eq (jabber:xform-type parsed-form) :submit))
	  (let ((number-field (jabber:find-field parsed-form "number")))
	    (when number-field
	      (let ((parsed-data (read-from-string (car (jabber:xform-field-values number-field)))))
		(when (numberp parsed-data)
		  (push parsed-data closure-data)))))))
      (values "executing" '("next" "prev" "complete") (list (make-form)) closure-data))
     ((eql action :prev)
      (pop closure-data)
      (values "executing" '("next" "prev" "complete") (list (make-form)) closure-data))
     ((eql action :cancel)
      (values "canceled" nil '(("note" (("type" "info")) "Command canceled.")) nil))
     ((eql action :complete)
      (let ((result-form
	     (make-instance 'jabber:xform
			    :type :result
			    :title "Result of addition"
			    :fields
			    (list
			     (make-instance 'jabber:xform-field
					    :var "number"
					    :label "Number")
			     (make-instance 'jabber:xform-field
					    :var "sum"
					    :label "Accumulated sum"))
			    :items
			    (let ((sum 0) items)
			      (dolist (n (reverse closure-data))
				(incf sum n)
				(push (list
				       (make-instance 'jabber:xform-field
						      :var "number"
						      :values (list (write-to-string n)))
				       (make-instance 'jabber:xform-field
						      :var "sum"
						      :values (list (write-to-string sum))))
				      items))
			      (nreverse items)))))
	(values "completed" nil `(("note" (("type" "info")) 
				   ,(format nil "Command completed.  The sum is ~A." (reduce #'+ closure-data)))
				  ,(jabber:output-xdata-form result-form))
		nil)))
     (t
      (error 'jabber:jabber-error :type "modify" :condition "bad-request"
	     :app-specific `(("malformed-action" . ,jabber:+adhoc-namespace+))
	     :text (format nil "Action '~A' not understood" action))))))

;; arch-tag: 18e96068-1256-11da-a46a-000a95c2fcd0

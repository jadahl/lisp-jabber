(in-package :jabber)

(define-condition unexpected-stanza-error (error)
  ((stanza :initarg :stanza :reader stanza))
  (:report (lambda (condition stream)
	     (format stream "Unexpected stanza: ~A." (stanza condition)))))

(define-condition jabber-error (error)
  ((condition :initarg :condition :reader jabber-error-condition
	      :initform (error "Condition required."))
   (text :initarg :text :reader text :initform nil)
   (type :initarg :type :reader jabber-error-type :initform nil)
   (app-specific :initarg :app-specific :reader app-specific :initform nil))
  (:report (lambda (condition stream)
	     (format stream "Jabber error: ~A~@[, ~A~]~@[, ~A~]."
		     (jabber-error-condition condition)
		     (text condition)
		     (app-specific condition))))
  (:documentation
   "Base class for stream and stanza errors."))

(define-condition auth-error (error)
  ((text :initarg :text :reader text :initform nil))
  (:report (lambda (condition stream)
	     (format stream "Authentication error: ~a." (text condition)))))

(define-condition auth-stanza-error (jabber-error auth-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Authentication error: ~A~@[, ~A~]~@[, ~A~]."
		     (jabber-error-condition condition)
		     (text condition)
		     (app-specific condition)))))

(define-condition conference-destroyed-error (error)
  ((text :initarg :text :reader text :initform nil))
  (:report (lambda (condition stream)
             (format stream "Room error: ~a." (text condition)))))

;; arch-tag: 41de3bd6-98dc-11d9-9930-000a95c2fcd0

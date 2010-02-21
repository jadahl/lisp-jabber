;;; -*- mode: lisp; -*-

(defpackage jabber-system
  (:use :common-lisp :asdf))

(in-package jabber-system)

(defsystem jabber
  :version "0.0.1"
  :licence "BSD-style"
  :depends-on (:xmls 
	       #-(or sbcl clisp) :port
	       #+sbcl :sb-bsd-sockets)
  :components
  ((:file "packages")
   (:file "errors" :depends-on ("packages"))
   (:file "stanzas" :depends-on ("packages" "errors"))
   (:file "utils" :depends-on ("packages"))
   (:file "connection" :depends-on ("stanzas" "utils" "errors"))
   (:file "xdata" :depends-on ("packages"))
   (:file "disco" :depends-on ("packages" "connection"))
   (:file "adhoc" :depends-on ("packages" "disco"))
   (:file "register" :depends-on ("packages"))))

(defmethod perform ((o test-op) (c (eql (find-system :jabber))))
  (operate 'load-op :jabber-tests)
  (operate 'test-op :jabber-tests))

(defsystem jabber-tests
  :depends-on (:jabber :rt)
  :components
  ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :jabber-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "RT")))
      (error "test-op failed")))

;; arch-tag: 4fdbb3c0-9595-11d9-831e-000a95c2fcd0

;;; -*- mode: lisp; -*-

(defpackage jabber-client-system
  (:use :common-lisp :asdf))

(in-package jabber-client-system)

(defsystem jabber-client
  :version "0.0.1"
  :licence "BSD-style"
  :depends-on (jabber cl-sasl cl-base64)
  :components
  ((:file "client")
   (:file "roster" :depends-on ("client"))))

;; arch-tag: b5475ac4-9596-11d9-831e-000a95c2fcd0

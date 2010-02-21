;;; -*- mode: lisp; -*-

(defpackage jabber-component-system
  (:use :common-lisp :asdf))

(in-package jabber-component-system)

(defsystem jabber-component
  :version "0.0.1"
  :licence "BSD-style"
  :depends-on (jabber sb-sha1)
  :components
  ((:file "component")))

;; arch-tag: efd3104e-9595-11d9-831e-000a95c2fcd0

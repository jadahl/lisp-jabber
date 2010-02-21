;;; -*- mode: lisp; -*-

(defpackage jabber-muc-system
  (:use :common-lisp :asdf))

(in-package jabber-muc-system)

(defsystem jabber-muc
  :version "0.0.1"
  :licence "BSD-style"
  :depends-on (jabber cl-sasl cl-base64)
  :components
  ((:file "conference")
   (:file "muc" :depends-on ("conference"))))

;; arch-tag: 73ea751f-9c1e-48a4-a2a1-8281be7bd2c0

(defpackage jabber-tests
  (:use :cl :jabber :rt))
(in-package :jabber-tests)

(deftest jid-username-1
  (jid-username "user@host")
  "user")

(deftest jid-username-2
  (jid-username "user@host/resource")
  "user")

(deftest jid-username-3
  (values (jid-username "host") (jid-username "host/resource"))
  nil nil)

(deftest jid-user-1
  (jid-user "user@host/resource")
  "user@host")

(deftest jid-user-2
  (jid-user "host/foo/bar")
  "host")

(deftest jid-server-1
  (jid-server "user@host")
  "host")

(deftest jid-server-2
  (jid-server "user@host/resource")
  "host")

(deftest jid-server-3
  (jid-server "host/resource")
  "host")

(deftest jid-server-4
  (jid-server "host")
  "host")

(deftest jid-resource-1
  (jid-resource "user@host/resource")
  "resource")

(deftest jid-resource-2
  (jid-resource "user@host/foo/bar")
  "foo/bar")

(deftest jid-equal-1 (jid-equal "User@host/foo" "user@Host/foo") t)
(deftest jid-equal-2 (jid-equal "user@host/Foo" "user@host/foo") nil)
(deftest jid-equal-3 (jid-equal "user@host" "user@host/foo") nil)

;; arch-tag: d50eaa63-0a7b-4388-aea2-cf71d6c6b857

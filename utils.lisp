(in-package :jabber)

(defun jid-username (jid)
  "From user@host or user@host/resource, extract user.
Else return nil."
  (let ((at-pos (position #\@ jid)))
    (when at-pos
      (subseq jid 0 at-pos))))

(defun jid-user (jid)
  "Remove resource from JID, if any."
  (subseq jid 0 (position #\/ jid)))

(defun jid-server (jid)
  "From user@host, user@host/resource or host/resource, extract host."
  (let* ((without-resource (jid-user jid))
	 (at-pos (position #\@ without-resource)))
    (if at-pos
	(subseq without-resource (1+ at-pos))
      without-resource)))

(defun jid-resource (jid)
  "From user@host/resource or host/resource, extract resource.
Else, return nil."
  (let ((start (position #\/ jid)))
    (when start
      (subseq jid (1+ start)))))

(defun normalize-username (node)
  "Normalize username part of JID."
  ;; XXX: really implement Nodeprep
  (string-downcase node))

(defun normalize-hostname (host)
  "Normalize hostname part of JID."
  ;; XXX: understand and implement proper normalization
  (string-downcase host))

(defun normalize-resource (resource)
  "Normalize resource part of JID."
  ;; XXX: really implement Resourceprep
  (copy-seq resource))

(defun jid-normalize (jid)
  "Normalize the given JID.
Normalized JIDs can be reliably compared with STRING=."
  (let ((username (jid-username jid))
	(host (jid-server jid))
	(resource (jid-resource jid)))
    (concatenate 'string
		 (when username
		   (format nil "~A@" (normalize-username username)))
		 (normalize-hostname host)
		 (when resource
		   (format nil "/~A" (normalize-resource resource))))))

(defun jid-equal (a b)
  "Return non-nil if JIDs A and B are equal in all components.
If you know that both JIDs are normalized, you can use STRING= instead."
  (string= (jid-normalize a) (jid-normalize b)))

;; arch-tag: aX0IS5PZwQ4gWqXSLAjERcWWJvvXob+XYxFNMUF8joM=

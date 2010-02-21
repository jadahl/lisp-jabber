;;; MUC - Multi-User-Chat

(in-package :jabber)

(defclass muc (connection)
  ((conferences :accessor muc-conferences
          :initform (make-hash-table :test #'equal))))

(defmethod join-conference ((c muc) (r conference))
  "Joins a conference given a muc instance (connection) and a conference instance."
  (let* ((conference-jid (format nil "~a@~a" (conference-name r) (conference-server r)))
         (jid (format nil "~a/~a" conference-jid (conference-nick r)))
         (attrs `(("to" ,jid)))
         (x-attrs '(("xmlns" "http://jabber.org/protocol/muc")))
         (join-stanza (xmls:make-node :name "presence" 
                                      :attrs attrs
                                      :child (xmls:make-node :name "x"
                                                             :attrs x-attrs))))
    (format t "~a~%" join-stanza)
    (send-stanza c join-stanza)
    (setf (conference-parent r) c)
    (setf (gethash conference-jid (muc-conferences c)) r)))

(defmethod joined-conference ((c muc) (r conference))
  "Called when conference is joined")

(defmethod send-muc-message ((c muc) conference-jid message)
  "Send a message to all occupants."
  (send-message c :to conference-jid :type "groupchat" :body message))

(defmethod handle-stanza :before ((c muc) (p presence))
  "Handle incoming presence stanzas, route suitable to muc mechanism"
  (let* ((jid (jid-user (stanza-from p)))
         (conference (gethash jid (muc-conferences c))))
    (when conference
      (handler-case
        (handle-conference-presence conference p)
        (conference-destroyed-error (condition)
                                    (conference-destroyed
                                      c
                                      (prog1
                                        (gethash jid (muc-conferences c))
                                        (format t "Conference was destroyed or you was unable to join: ~a~%" condition)
                                        (remhash jid (muc-conferences c)))
                                      condition))))))

(defmethod get-conference ((c muc) conference-jid)
  (gethash conference-jid (muc-conferences c)))

(defmethod conference-destroyed ((c muc) (r conference) reason)
  )

;;; arch-id: 5c97d811-8209-4854-a30b-814acd643616

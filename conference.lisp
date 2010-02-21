;;; MUC Room 

(in-package :jabber)

(defclass participant ()
  ((nick     :accessor participant-nick
             :initarg :nick)
   (real-jid :accessor participant-real-jid
             :initarg :real-jid
             :initform nil)))

(defclass conference ()
  ((parent :accessor conference-parent
           :initarg :parent
           :initform nil)
   (name :accessor conference-name
         :initarg :name)
   (server :accessor conference-server
           :initarg :server)
   (nick :accessor conference-nick
         :initarg :nick)
   (participants :accessor conference-participants
                 :initform (make-hash-table :test #'equal))))

(defmethod remove-participant ((r conference) nick)
  (remhash nick (conference-participants r)))

(defmethod new-participant ((r conference) nick)
  (if (string-equal (conference-nick r) nick)
    (conference-joined r))
  (setf (gethash nick (conference-participants r)) (make-instance 'participant :nick nick)))

(defmethod conference-joined ((r conference))
  (joined-conference (conference-parent r) r))

(defmethod own-conference-jid ((r conference))
  (format nil "~a@~a/~a" (conference-name r) (conference-server r) (conference-nick r)))

(defmethod handle-conference-presence ((r conference) (p presence))
  "Handle incoming presence stanzas from a conference"
  ;; handle error
  (cond
    ((string-equal (stanza-type p) "error")
     (error 'conference-destroyed-error :text (xmls:node-name (stanza-node p))))
    ((string-equal (stanza-type p) "unavailable")
     ;; check if we are being destroyed
     (when (string-equal (own-conference-jid r) (stanza-from p))
       (let* ((x (find "x" (stanza-children p) 
                       :test #'string-equal :key #'xmls:node-name))
              (status (find "status" x 
                            :test #'string-equal :key #'xmls:node-name))
              (code (xmls:get-attr status "code")))
         (cond
           ((not (string-equal code "303")) ;; nick change
            (error 'conference-destroyed-error :text code)))))
     (remove-participant r (jid-resource (stanza-from p))))
    ((or (string-equal (stanza-type p) "available") (not (stanza-type p)))
     (new-participant r (jid-resource (stanza-from p))))))

;;; arch-id: fbcc4aba-cf74-4589-add2-93286e6ec245

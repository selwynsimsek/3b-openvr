;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings to the IVRNotifications API.

;;; IVR_Notifications_002

;; according to openvr.h this interface is not implemented yet anyway...

(in-package 3b-openvr)

(defclass notification ()
  ((id :initarg :id :accessor id)
   (removed-p :initarg :removed-p :accessor removed-p)))

(defun create-notification (overlay user-value type text style image &key (notifications *notifications*))
  (error "implement me"))

(defun remove-notification (notification &key (notifications *notifications*))
  (%remove-notification (table notifications) (id notification)))

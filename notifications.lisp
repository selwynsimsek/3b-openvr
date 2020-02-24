;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings to the IVRNotifications API.

;;; IVR_Notifications_002

;; according to openvr.h this interface is not implemented yet anyway...

(in-package 3b-openvr)

(defmacro with-notification-error (&rest body)
  (let ((error-name (gensym "error-value")))
    `(let ((,error-name (progn ,@body)))
       (if (eq ,error-name :ok) t (error "VR overlay error: ~a" ,error-name)))))

(defclass notification ()
  ((id :initarg :id :accessor id)
   (removed-p :initarg :removed-p :accessor removed-p :initform nil)
   (overlay-handle :initarg :overlay-handle :accessor overlay-handle)
   (user-value :initarg :user-value :accessor user-value)
   (notification-type :initarg :notification-type :accessor notification-type)
   (text :initarg :text :accessor text)
   (style :initarg :style :accessor style)
   (image :initarg :image :accessor image)))

(defclass notification-bitmap ()
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (bytes-per-pixel :initarg :bytes-per-pixel :accessor bytes-per-pixel)
   (image-buffer :initarg :image-buffer :accessor image-buffer)))

(defun create-notification (overlay-handle user-value type text style image &key (notifications *notifications*))
  "Create a notification and enqueue it to be shown to the user.
   An overlay handle is required to create a notification, as otherwise it would be impossible for a user to act on it.
   To create a two-line notification, use a line break ('\n') to split the text into two lines.
   The image argument may be NIL, in which case the specified overlay's icon will be used instead."
  (cffi:with-foreign-objects ((pointer 'vr-notification-id)
                              (bitmap '(:struct notification-bitmap-t)))
    (if image
        (with-notification-error
          (cffi:with-pointer-to-vector-data (raw-pointer (image-buffer image))
            (setf (cffi:foreign-slot-value bitmap '(:struct notification-bitmap-t) 'width)
                  (width image)
                  (cffi:foreign-slot-value bitmap '(:struct notification-bitmap-t) 'height)
                  (height image)
                  (cffi:foreign-slot-value bitmap '(:struct notification-bitmap-t) 'bytes-per-pixel)
                  (bytes-per-pixel image)
                  (cffi:foreign-slot-value bitmap '(:struct notification-bitmap-t) 'image-data)
                  raw-pointer)
            (%create-notification (table notifications) overlay-handle user-value type text style
                                  bitmap pointer)))
        (with-notification-error
            (%create-notification (table notifications) overlay-handle user-value type text style
                                  (cffi:null-pointer) pointer)))
    (make-instance 'notification :removed-p nil :id (cffi:mem-ref pointer 'vr-notification-id)
                                 :overlay-handle overlay-handle :user-value user-value :notification-type type
                                 :text text :style style :image image)))

(defun remove-notification (notification &key (notifications *notifications*))
  "Destroy a notification, hiding it first if it currently shown to the user."
  (with-notification-error
      (%remove-notification (table notifications) (id notification))
    (setf (removed-p notification) t)
    t))

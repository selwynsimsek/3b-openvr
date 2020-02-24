;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level API for the IVRDriverManager API

;;; IVR_DriverManager_001

(defun driver-count (&key (driver-manager *driver-manager*))
  (%get-driver-count (table driver-manager)))

(defun driver-name (driver-id &key (driver-manager *driver-manager*) (buffer-size 512))
  (cffi:with-foreign-string (buffer (make-string buffer-size :initial-element #\space))
    (%get-driver-name (table driver-manager) driver-id buffer buffer-size)
    (cffi:foreign-string-to-lisp buffer)))

(defun driver-handle (name &key (driver-manager *driver-manager*))
  (%get-driver-handle (table driver-manager) name))

(defun driver-enabled-p (driver-id &key (driver-manager *driver-manager*))
  (%is-enabled (table driver-manager) driver-id))

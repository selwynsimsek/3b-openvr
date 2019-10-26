;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level API for the IVRDriverManager API

;;; IVR_DriverManager_001

(defun driver-count (&key (driver-manager *driver-manager*))
  (%get-driver-count (table driver-manager)))

(defun driver-name (driver-id &key (driver-manager *driver-manager*))
  (cffi:with-foreign-string (buffer (make-string 512 :initial-element #\space))
    (%get-driver-name (table driver-manager) driver-id buffer 512)
    (cffi:foreign-string-to-lisp buffer)))

(defun driver-handle (name &key (driver-manager *driver-manager*))
  (%get-driver-handle (table driver-manager) name))

(defun driver-enabled-p (driver-id &key (driver-manager *driver-manager*))
  (error "Not implemented yet")) ; where is the function?

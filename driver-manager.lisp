;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level API for the IVRDriverManager API

;;; IVR_DriverManager_001

(in-package :3b-openvr)





(defun driver-count (&key (driver-manager *driver-manager*))
  (%get-driver-count (table driver-manager))) ; works


(defun driver-name (driver-id &key (driver-manager *driver-manager*) (buffer-size 512))
  (cffi:with-foreign-string (buffer (make-string buffer-size :initial-element #\space))
    (%get-driver-name (table driver-manager) driver-id buffer buffer-size)
    (cffi:foreign-string-to-lisp buffer))) ; works


(defun driver-names (&key (driver-manager *driver-manager*))
  (coerce (loop for i from 0 below (driver-count :driver-manager driver-manager)
                collect (driver-name i :driver-manager driver-manager))
          'vector)) ; works


(defun driver-handle (name &key (driver-manager *driver-manager*))
  (%get-driver-handle (table driver-manager) name)) ; works


(defun driver-enabled-p (driver-id &key (driver-manager *driver-manager*))
  (%is-enabled (table driver-manager) driver-id)) ; works

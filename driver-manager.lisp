;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level API for the IVRDriverManager API

;;; IVR_DriverManager_001

(in-package :3b-openvr)

(annot:enable-annot-syntax)


@export
(defun driver-count (&key (driver-manager *driver-manager*))
  (%get-driver-count (table driver-manager)))

@export
(defun driver-name (driver-id &key (driver-manager *driver-manager*) (buffer-size 512))
  (cffi:with-foreign-string (buffer (make-string buffer-size :initial-element #\space))
    (%get-driver-name (table driver-manager) driver-id buffer buffer-size)
    (cffi:foreign-string-to-lisp buffer)))

@export
(defun driver-handle (name &key (driver-manager *driver-manager*))
  (%get-driver-handle (table driver-manager) name))

@export
(defun driver-enabled-p (driver-id &key (driver-manager *driver-manager*))
  (%is-enabled (table driver-manager) driver-id))

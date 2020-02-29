;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the Settings API.

;;; IVRSettings_003

(in-package :3b-openvr)

(annot:enable-annot-syntax)

@export
(defun remove-section (section &key (settings *settings*))
  (cffi:with-foreign-object (error-pointer 'vr-settings-error)
    (%remove-section (table settings) section error-pointer)
    (let ((error-code (cffi:mem-ref error-pointer 'vr-settings-error)))
      (unless (eq error-code :none)
        (error "Settings error: ~a" error-code)))))

@export
(defun remove-key-in-section (section settings-key &key (settings *settings*))
  (cffi:with-foreign-object (error-pointer 'vr-settings-error)
    (%remove-key-in-section (table settings) section settings-key error-pointer)
    (let ((error-code (cffi:mem-ref error-pointer 'vr-settings-error)))
      (unless (eq error-code :none)
        (error "Settings error: ~a" error-code)))))

@export
(defun settings-set (section settings-key value type &key (settings *settings*))
  (cffi:with-foreign-object (error-pointer 'vr-settings-error)
    (cond
      ((or (eq type :int) (eq type :int32))
       (%set-int32 (table settings) section settings-key value error-pointer))
      ((eq type :float)
       (%set-float (table settings) section settings-key value error-pointer))
      ((or (eq type :boolean) (eq type :bool))
       (%set-bool (table settings) section settings-key value error-pointer))
      ((eq type :string)
       (%set-string (table settings) section settings-key value error-pointer)))
    (let ((error-code (cffi:mem-ref error-pointer 'vr-settings-error)))
      (unless (eq error-code :none)
        (error "Settings error: ~a" error-code)))))

@export
(defun settings-get (section settings-key type &key (settings *settings*))
  (cffi:with-foreign-object (error-pointer 'vr-settings-error)
    (prog1
        (cond
          ((or (eq type :int) (eq type :int32))
           (%get-int32 (table settings) section settings-key error-pointer))
          ((eq type :float)
           (%get-float (table settings) section settings-key error-pointer))
          ((or (eq type :boolean) (eq type :bool))
           (%get-bool (table settings) section settings-key error-pointer))
          ((eq type :string)
           (cffi:with-foreign-string (string-pointer (make-string 512))
             (%get-string (table settings) section settings-key string-pointer 512 error-pointer)
             (cffi:foreign-string-to-lisp string-pointer))))
      (let ((error-code (cffi:mem-ref error-pointer 'vr-settings-error)))
        (unless (eq error-code :none)
          (error "Settings error: ~a" error-code))))))

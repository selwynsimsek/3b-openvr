;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the Settings API.

;;; IVRSettings_003

(in-package :3b-openvr)

(defun remove-section (section &key (settings *settings*))
  (cffi:with-foreign-object (error-pointer 'vr-settings-error)
    (%remove-section (table settings) section error-pointer)
    (let ((error-code (cffi:mem-ref error-pointer 'vr-settings-error)))
      (unless (eq error-code :none)
        (error "Settings error: ~a" error-code))))) ; doesn't work?


(defun remove-key-in-section (section settings-key &key (settings *settings*))
  (cffi:with-foreign-object (error-pointer 'vr-settings-error)
    (%remove-key-in-section (table settings) section settings-key error-pointer)
    (let ((error-code (cffi:mem-ref error-pointer 'vr-settings-error)))
      (unless (eq error-code :none)
        (error "Settings error: ~a" error-code))))) ; doesn't work?


(defun settings-set (section settings-key value type &key (settings *settings*))
  (cffi:with-foreign-object (error-pointer 'vr-settings-error)
    (setf (cffi:mem-ref error-pointer 'vr-settings-error) :none)
    (cond
      ((or (eq type :int) (eq type :int32))
       (%set-int32 (table settings) section settings-key value error-pointer))
      ((eq type :float)
       (%set-float (table settings) section settings-key value error-pointer))
      ((or (eq type :boolean) (eq type :bool))
       (progn (print type)
              (%set-bool (table settings) section settings-key value error-pointer)))
      ((eq type :string)
       (%set-string (table settings) section settings-key value error-pointer)))
    (let ((error-code (cffi:mem-ref error-pointer 'vr-settings-error)))
      (print error-code)
      (unless (eq error-code :none)
        (error "Settings error: ~a" error-code))))) ; doesn't work?


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
          (error "Settings error: ~a" error-code)))))) ; works

(defun settings (section settings-key &key (settings *settings*))
  (handler-case (settings-get section settings-key :int :settings settings)
    (t () (handler-case (settings-get section settings-key :float :settings settings)
            (t () (handler-case (settings-get section settings-key :boolean :settings settings)
                    (t () (settings-get section settings-key :string :settings settings))))))))
; works

(defmethod (setf settings) ((value integer) section settings-key &key (settings *settings*))
  (settings-set section settings-key value :int :settings settings))

(defmethod (setf settings) ((value float) section settings-key &key (settings *settings*))
  (settings-set section settings-key value :float :settings settings))

(defmethod (setf settings)  ((value symbol) section settings-key &key (settings *settings*))
  (settings-set section settings-key value :boolean :settings settings))

(defmethod (setf settings) ((value string) section settings-key &key (settings *settings*))
  (settings-set section settings-key value :string :settings settings))

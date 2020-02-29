;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the Applications API.

;;; IVR_Applications_007

(in-package :3b-openvr)

(annot:enable-annot-syntax)

;; application management

@export
(defun add-application-manifest (manifest-path &key (temporary-p nil)
                                                    (applications *applications*))
  (%add-application-manifest (table applications) manifest-path temporary-p))

@export
(defun remove-application-manifest (manifest-path &key (applications *applications*))
  (%remove-application-manifest (table applications) manifest-path))

@export
(defun application-installed-p (application-key &key (applications *applications*))
  (%is-application-installed (table applications) application-key))

@export
(defun application-keys (&key (applications *applications*))
  (let* ((count (%get-application-count (table applications)))
         (result-array (make-array (list count))))
    (loop for i from 0 below count
          do (setf (aref result-array i)
                   (cffi:with-foreign-string (foreign-pointer +max-application-key-length+)
                     (%get-application-key-by-index (table applications)
                                                    i
                                                    foreign-pointer
                                                    +max-application-key-length+)
                     (cffi:foreign-string-to-lisp foreign-pointer)))
          finally (return result-array))))

@export
(defun application-key-by-process-id (process-id &key (applications *applications*))
  (cffi:with-foreign-string (foreign-pointer +max-application-key-length+)
    (%get-application-key-by-process-id (table applications)
                                        process-id
                                        foreign-pointer
                                        +max-application-key-length+)
    (cffi:foreign-string-to-lisp foreign-pointer)))

@export
(defun launch-application (application-key &key (applications *applications*))
  (%launch-application (table applications) application-key))

@export
(defclass application-override-key ()
  ((key :accessor key :initarg :key)
   (value :accessor value :initarg :value)))

@export
(defun launch-template-application (template-application-key new-application-key
                                    app-override-keys &key (applications *applications*))
  (cffi:with-foreign-object (foreign-keys '(:struct app-override-keys-t) (length app-override-keys))
    (loop for i from 0 below (length app-override-keys) do
          (let ((key (cffi:mem-aref foreign-keys '(:struct app-override-keys-t) i)))
            (cffi:with-foreign-slots ((key value) key '(:struct app-override-keys-t))
              (setf key (key (aref app-override-keys i))
                    value (value (aref app-override-keys i))))))
    (%launch-template-application (table applications) template-application-key new-application-key
                                  foreign-keys (length app-override-keys))))

@export
(defun launch-application-from-mime-type (mime-type args &key (applications *applications*))
  (%launch-application-from-mime-type (table applications) mime-type args))

@export
(defun launch-dashboard-overlay (application-key &key (applications *applications*))
  (%launch-dashboard-overlay (table applications) application-key))

@export
(defun cancel-application-launch (application-key &key (applications *applications*))
  (%cancel-application-launch (table application-key) application-key))

@export
(defun identify-application (process-id application-key &key (applications *applications*))
  (%identify-application (table applications) process-id application-key))

@export
(defun application-process-id (application-key &key (applications *applications*))
  (%get-application-process-id (table applications) application-key))



(defun application-property (application-key application-property ;incomplete
                             &key (applications *applications*))
  (cond ((eq application-property :last-launch-time)
         (%get-application-property-uint64 (table applications)
                                           application-key
                                           :last-launch-time-uint64))
        ))

@export
(defun set-application-auto-launch (application-key auto-launch-p
                                    &key (applications *applications*))
  (%set-application-auto-launch (table applications) application-key auto-launch-p))

@export
(defun application-auto-launch-p (application-key &key (applications *applications*))
  (%get-application-auto-launch (table applications) application-key))

@export
(defun set-default-application-for-mime-type
    (application-key mime-type &key (applications *applications*))
  (%set-default-application-for-mime-type (table applications) application-key mime-type))

@export
(defun default-application-for-mime-type (mime-type &key (applications *applications*))
  (cffi:with-foreign-string (foreign-string (make-string +max-application-key-length+))
    (%get-default-application-for-mime-type (table applications)
                                            mime-type
                                            foreign-string
                                            +max-application-key-length+)
    (cffi:foreign-string-to-lisp foreign-string)))

@export
(defun application-supported-mime-types (application-key &key (applications *applications*))
  (cffi:with-foreign-string (foreign-string (make-string 512))
    (%get-application-supported-mime-types (table applications)
                                           foreign-string
                                           512)
    (cffi:foreign-string-to-lisp foreign-string)))

@export
(defun applications-that-support-mime-type (mime-type &key (applications *applications*))
  (let ((length (%get-applications-that-support-mime-type (table applications)
                                                          mime-type (cffi:null-pointer) 0)))
    (cffi:with-foreign-string (foreign-string (make-string length))
      (%get-applications-that-support-mime-type (table applications) mime-type foreign-string length)
      (cffi:foreign-string-to-lisp foreign-string))))

@export
(defun application-launch-arguments (handle &key (applications *applications*)
                                                 (buffer-size 2048))
  "Get the args list from an app launch that had the process already running, you call this when you get a
   VREvent_ApplicationMimeTypeLoad"
  (cffi:with-foreign-string (pointer (make-string buffer-size))
    (%get-application-launch-arguments (table applications) handle pointer buffer-size)))


;; transition methods

@export
(defun starting-application (&key (applications *applications*))
  (cffi:with-foreign-string (pointer (make-string +max-application-key-length+))
    (%get-starting-application (table applications) pointer +max-application-key-length+)
    (cffi:foreign-string-to-lisp pointer)))

@export
(defun transition-state (&key (applications *applications*))
  (%get-transition-state (table applications)))

@export
(defun perform-application-prelaunch-check (application-key &key (applications *applications*))
  (%perform-application-prelaunch-check (table applications) application-key))

@export
(defun quit-user-prompt-requested-p (&key (applications *applications*))
  (%is-quit-user-prompt-requested (table applications)))

@export
(defun launch-internal-process (binary-path arguments working-directory
                                &key (applications *applications*))
  (%launch-internal-process (table applications) binary-path arguments working-directory))

@export
(defun current-scene-process-id (&key (applications *applications*))
  (%get-current-scene-process-id (table applications)))

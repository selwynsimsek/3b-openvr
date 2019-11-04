;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the Applications API.

;;; IVR_Applications_006

(in-package :3b-openvr)

;; application management

(defun add-application-manifest (manifest-path &key (temporary-p nil)
                                                    (applications *applications*))
  (%add-application-manifest (table applications) manifest-path temporary-p))

(defun remove-application-manifest (manifest-path &key (applications *applications*))
  (%remove-application-manifest (table applications) manifest-path))

(defun application-installed-p (application-key &key (applications *applications*))
  (%is-application-installed (table applications) application-key))

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

(defun application-key-by-process-id (process-id &key (applications *applications*))
  (cffi:with-foreign-string (foreign-pointer +max-application-key-length+)
    (%get-application-key-by-process-id (table applications)
                                        process-id
                                        foreign-pointer
                                        +max-application-key-length+)
    (cffi:foreign-string-to-lisp foreign-pointer)))

(defun launch-application (application-key &key (applications *applications*))
  (%launch-application (table applications) application-key))

(defun launch-template-application ()
  (error "implement me"))

(defun launch-application-from-mime-type (mime-type args &key (applications *applications*))
  (%launch-application-from-mime-type (table applications) mime-type args))

(defun launch-dashboard-overlay (application-key &key (applications *applications*))
  (%launch-dashboard-overlay (table applications) application-key))

(defun cancel-application-launch (application-key &key (applications *applications*))
  (%cancel-application-launch (table application-key) application-key))

(defun identify-application (process-id application-key &key (applications *applications*))
  (%identify-application (table applications) process-id application-key))

(defun application-process-id (application-key &key (applications *applications*))
  (%get-application-process-id (table applications) application-key))


;; application properties

(defun application-property (application-key application-property
                             &key (applications *applications*))
  (error "implement me"))

(defun set-application-auto-launch (application-key auto-launch-p
                                    &key (applications *applications*))
  (%set-application-auto-launch (table applications) application-key auto-launch-p))

(defun application-auto-launch-p (application-key &key (applications *applications*))
  (%get-application-auto-launch (table applications) application-key))

(defun set-default-application-for-mime-type
    (application-key mime-type &key (applications *applications*))
  (%set-default-application-for-mime-type (table applications) application-key mime-type))

(defun default-application-for-mime-type (mime-type &key (applications *applications*))
  (cffi:with-foreign-string (foreign-string (make-string +max-application-key-length+))
    (%get-default-application-for-mime-type (table applications)
                                            mime-type
                                            foreign-string
                                            +max-application-key-length+)
    (cffi:foreign-string-to-lisp foreign-string)))

(defun application-supported-mime-types (application-key &key (applications *applications*))
  (cffi:with-foreign-string (foreign-string (make-string 512))
    (%get-application-supported-mime-types (table applications)
                                           foreign-string
                                           512)
    (cffi:foreign-string-to-lisp foreign-string)))

(defun applications-that-support-mime-type (mime-type &key (applications *applications*))
  (error "implement me"))

(defun application-launch-arguments (handle &key (applications *applications*))
  (error "implement me"))


;; transition methods

(defun starting-application (&key (applications *applications*))
  (cffi:with-foreign-string (pointer (make-string +max-application-key-length+))
    (%get-starting-application (table applications) pointer +max-application-key-length+)
    (cffi:foreign-string-to-lisp pointer)))

(defun transition-state (&key (applications *applications*))
  (%get-transition-state (table applications)))

(defun perform-application-prelaunch-check (application-key &key (applications *applications*))
  (%perform-application-prelaunch-check (table applications) application-key))

(defun quit-user-prompt-requested-p (&key (applications *applications*))
  (%is-quit-user-prompt-requested (table applications)))

(defun launch-internal-process (binary-path arguments working-directory
                                &key (applications *applications*))
  (%launch-internal-process (table applications) binary-path arguments working-directory))

(defun current-scene-process-id (&key (applications *applications*))
  (%get-current-scene-process-id (table applications)))

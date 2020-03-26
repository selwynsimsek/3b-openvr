;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the Applications API.

;;; IVR_Applications_007

(in-package :3b-openvr)



;; application management


(defun add-application-manifest (manifest-path &key (temporary-p nil)
                                                    (applications *applications*))
  (%add-application-manifest (table applications) manifest-path temporary-p))


(defun remove-application-manifest (manifest-path &key (applications *applications*))
  (%remove-application-manifest (table applications) manifest-path))


(defun application-installed-p (application-key &key (applications *applications*))
  (%is-application-installed (table applications) application-key)) ; works


(defun application-keys (&key (applications *applications*))
  (let* ((count (%get-application-count (table applications)))
         (result-array (make-array (list count))))
    (loop for i from 0 below count
          do (setf (aref result-array i)
                   (cffi:with-foreign-string (foreign-pointer (make-string +max-application-key-length+))
                     (%get-application-key-by-index (table applications)
                                                    i
                                                    foreign-pointer
                                                    +max-application-key-length+)
                     (cffi:foreign-string-to-lisp foreign-pointer)))
          finally (return result-array)))) ; works


(defun application-key-by-process-id (process-id &key (applications *applications*))
  (cffi:with-foreign-string (foreign-pointer (make-string +max-application-key-length+))
    (%get-application-key-by-process-id (table applications)
                                        process-id
                                        foreign-pointer
                                        +max-application-key-length+)
    (cffi:foreign-string-to-lisp foreign-pointer)))


(defun launch-application (application-key &key (applications *applications*))
  (%launch-application (table applications) application-key))


(defclass application-override-key ()
  ((key :accessor key :initarg :key)
   (value :accessor value :initarg :value)))


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


(defun launch-application-from-mime-type (mime-type args &key (applications *applications*))
  (%launch-application-from-mime-type (table applications) mime-type args))


(defun launch-dashboard-overlay (application-key &key (applications *applications*))
  (%launch-dashboard-overlay (table applications) application-key))


(defun cancel-application-launch (application-key &key (applications *applications*))
  (%cancel-application-launch (table application-key) application-key))


(defun identify-application (process-id application-key &key (applications *applications*))
  (%identify-application (table applications) process-id application-key))


(defun application-process-id (application-key &key (applications *applications*))
  (%get-application-process-id (table applications) application-key)) ; works


(defun application-property-with-type (application-key application-property type
                                       &key (applications *applications*))
  (cffi:with-foreign-object (error-pointer 'vr-application-error)
    (cond
      ((eq type :uint64)
       (let ((result
               (%get-application-property-uint64 (table applications)
                                                 application-key application-property error-pointer
                                                 )))
         (if (eq :none (cffi:mem-ref error-pointer 'vr-application-error))
                 result
                 (error "VR application error: ~a" (cffi:mem-ref error-pointer 'vr-application-error)))))
      ((eq type :bool)
       (let ((result
               (%get-application-property-bool (table applications)
                                               application-key application-property error-pointer)))
         (if (eq :none (cffi:mem-ref error-pointer 'vr-application-error))
             result
             (error "VR application error: ~a" (cffi:mem-ref error-pointer 'vr-application-error)))))
      ((eq type :string)
       (let ((string-length (%get-application-property-string
                             (table applications) application-key application-property
                             (cffi:null-pointer) 0 error-pointer)))
         (cffi:with-foreign-string (foreign-string-pointer (make-string string-length))
           (%get-application-property-string (table applications)
                                             application-key application-property
                                             foreign-string-pointer string-length error-pointer)
           (if (eq :none (cffi:mem-ref error-pointer 'vr-application-error))
               (cffi:foreign-string-to-lisp foreign-string-pointer)
               (error "VR application error: ~a" (cffi:mem-ref error-pointer 'vr-application-error)))))))))


(defun application-property (application-key application-property ;incomplete
                             &key (applications *applications*))
  (ecase application-property
    (:name (application-property-with-type application-key :name-string :string))
    (:launch-type (application-property-with-type application-key :launch-type-string :string))
    (:working-directory (application-property-with-type application-key :working-directory-string :string))
    (:binary-path (application-property-with-type application-key :binary-path-string :string))
    (:arguments (application-property-with-type application-key :arguments-string :string))
    (:url (application-property-with-type application-key :url-string :string))
    (:description (application-property-with-type application-key :description-string :string))
    (:news-url (application-property-with-type application-key :news-url-string :string))
    (:image-path (application-property-with-type application-key :image-path-string :string))
    (:source (application-property-with-type application-key :source-string :string))
    (:action-manifest-url (application-property-with-type application-key :action-manifest-url-string :string))
    (:is-dashboard-overlay (application-property-with-type application-key :is-dashboard-overlay-bool :bool))
    (:is-template (application-property-with-type application-key :is-template-bool :bool))
    (:is-instanced (application-property-with-type application-key :is-instanced-bool :bool))
    (:is-internal (application-property-with-type application-key :is-internal-bool :bool))
    (:wants-compositor-pause-in-standby (application-property-with-type
                                         application-key :wants-compositor-pause-in-standby-bool :bool))
    (:is-hidden (application-property-with-type
                 application-key :is-hidden-bool :bool))
    (:last-launch-time (application-property-with-type
                        application-key :last-launch-time-uint64 :uint64)))) ; works


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
                                           application-key
                                           foreign-string
                                           512)
    (cffi:foreign-string-to-lisp foreign-string)))


(defun applications-that-support-mime-type (mime-type &key (applications *applications*))
  (let ((length (%get-applications-that-support-mime-type (table applications)
                                                          mime-type (cffi:null-pointer) 0)))
    (cffi:with-foreign-string (foreign-string (make-string length))
      (%get-applications-that-support-mime-type (table applications) mime-type foreign-string length)
      (cffi:foreign-string-to-lisp foreign-string)))) ; doesn't work


(defun application-launch-arguments (handle &key (applications *applications*)
                                                 (buffer-size 2048))
  "Get the args list from an app launch that had the process already running, you call this when you get a
   VREvent_ApplicationMimeTypeLoad"
  (cffi:with-foreign-string (pointer (make-string buffer-size))
    (%get-application-launch-arguments (table applications) handle pointer buffer-size)))


;; transition methods


(defun starting-application (&key (applications *applications*))
  (cffi:with-foreign-string (pointer (make-string +max-application-key-length+))
    (%get-starting-application (table applications) pointer +max-application-key-length+)
    (cffi:foreign-string-to-lisp pointer)))


(defun scene-application-state (&key (applications *applications*))
  (%get-scene-application-state (table applications))) ; works


(defun perform-application-prelaunch-check (application-key &key (applications *applications*))
  (%perform-application-prelaunch-check (table applications) application-key))


(defun launch-internal-process (binary-path arguments working-directory
                                &key (applications *applications*))
  (%launch-internal-process (table applications) binary-path arguments working-directory))


(defun current-scene-process-id (&key (applications *applications*))
  (%get-current-scene-process-id (table applications))) ; works

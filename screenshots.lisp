(in-package 3b-openvr)

(defun request-screenshot (&key (preview-pathname (merge-pathnames #p"preview.png" (user-homedir-pathname)))
                                (vr-pathname (merge-pathnames #p"screenshot.png" (user-homedir-pathname)))
                                (type :stereo) (screenshots *screenshots*))
  "Requests a screenshot. Returns a handle to the screenshot."
  (cffi:with-foreign-object (handle 'screenshot-handle-t)
    (%request-screenshot (table screenshots) handle type (namestring preview-pathname) (namestring vr-pathname))
    (cffi:mem-ref handle 'screenshot-handle-t)))

(defun hook-screenshot (screenshot-type-list &key (screenshots *screenshots*))
  "Called by the running VR application to indicate which screenshots it wishes to support."
  (let ((number-of-types (length screenshot-type-list))) ; doesn't work yet!
    (cffi:with-foreign-object (supported-types 'vr-screenshot-type number-of-types)
      (loop for i below number-of-types
            do (setf (cffi:mem-aref supported-types 'vr-screenshot-type i)
                     (nth i screenshot-type-list)))
      (%hook-screenshot (table screenshots) supported-types number-of-types))))

(defun screenshot-property-type (handle &key (screenshots *screenshots*))
  "Returns the type of the screenshot specified by handle."
  (cffi:with-foreign-object (screenshot-error 'vr-screenshot-error)
    (prog1 (%get-screenshot-property-type (table screenshots) handle screenshot-error)
      (unless (eq :none (cffi:mem-ref screenshot-error 'vr-screenshot-error
                                      ))
        (error "VR screenshot error ~a" (cffi:mem-ref screenshot-error 'vr-screenshot-error))))))

(defun screenshot-property-filename (handle filename-type &key (screenshots *screenshots*))
  (cffi:with-foreign-string (buffer (make-string 512 :initial-element #\space))
    (cffi:with-foreign-object (error-pointer 'vr-screenshot-error) 
      (%get-screenshot-property-filename (table screenshots) handle filename-type buffer 512
                                         error-pointer)
      (cffi:foreign-string-to-lisp buffer))))

(defun update-screenshot-progress (handle progress &key (screenshots *screenshots*))
  "Present the user with a progress display during screenshot generation."
  (let ((error-value (%update-screenshot-progress (table screenshots) handle progress)))
    (unless (eq error-value :none)
      (error "VR screenshot error ~a" error-value))))

(defun take-stereo-screenshot (preview-pathname vr-pathname &key (screenshots *screenshots*))
  "Request a stereoscopic screenshot."
  (cffi:with-foreign-object (handle '(:pointer screenshot-handle-t))
    (let ((error-value
            (%take-stereo-screenshot (table screenshots)
                                     handle (namestring preview-pathname) (namestring vr-pathname))))
      (unless (eq error-value :none) (error "VR screenshot error ~a" error-value))
      (cffi:mem-ref handle 'screenshot-handle-t))))

(defun submit-screenshot (handle screenshot-type preview-pathname vr-pathname &key (screenshots *screenshots*))
  "Submit a new screenshot to the Steam API."
  (let ((error-value
          (%submit-screenshot (table screenshots) handle screenshot-type (namestring preview-pathname)
                              (namestring vr-pathname))))
    (unless (eq error-value :none) (error "VR screenshot error ~a" error-value))))

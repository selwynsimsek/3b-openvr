;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High level bindings for the IVRScreenshots interface.
;;; https://github.com/ValveSoftware/openvr/wiki/IVRScreenshots_Overview

;;; IVRScreenshots_001

(in-package 3b-openvr)



(defmacro with-screenshot-error (&rest body)
  (let ((error-name (gensym "error-value")))
    `(let ((,error-name (progn ,@body)))
       (if (eq ,error-name :none) t (error "VR overlay error: ~a" ,error-name)))))


(defclass screenshot ()
  ((handle :initarg :handle :accessor handle)
   (screenshot-type :initarg :screenshot-type :accessor screenshot-type)
   (preview-pathname :initarg :preview-pathname :accessor preview-pathname)
   (vr-pathname :initarg :vr-pathname :accessor vr-pathname)))


(defun screenshot-from-handle (handle &key (screenshots *screenshots*))
  (make-instance 'screenshot
                 :handle (cffi:mem-ref handle 'screenshot-handle-t)
                 :screenshot-type (screenshot-property-type
                                   (cffi:mem-ref handle 'screenshot-handle-t)
                                   :screenshots screenshots)
                 :preview-pathname (screenshot-property-filename
                                    (cffi:mem-ref handle 'screenshot-handle-t)
                                    :preview :screenshots screenshots)
                 :vr-pathname (screenshot-property-filename
                               (cffi:mem-ref handle 'screenshot-handle-t)
                               :vr :screenshots screenshots))) ; works


(defun request-screenshot (&key (preview-pathname #p"preview.png")
                                (vr-pathname #p"screenshot.png")
                                (type :stereo) (screenshots *screenshots*))
  "Requests a screenshot. Returns a handle to the screenshot."
  (cffi:with-foreign-object (handle 'screenshot-handle-t)
    (with-screenshot-error 
        (%request-screenshot (table screenshots) handle type
                             (namestring preview-pathname) (namestring vr-pathname)))
    (screenshot-from-handle handle))) ; works


(defun hook-screenshot (screenshot-type-list &key (screenshots *screenshots*))
  "Called by the running VR application to indicate which screenshots it wishes to support."
  (let ((number-of-types (length screenshot-type-list)))
    (cffi:with-foreign-object (supported-types 'vr-screenshot-type number-of-types)
      (loop for i below number-of-types
            do (setf (cffi:mem-aref supported-types 'vr-screenshot-type i)
                     (nth i screenshot-type-list)))
      (with-screenshot-error
          (%hook-screenshot (table screenshots) supported-types number-of-types))))) ; works


(defun screenshot-property-type (handle &key (screenshots *screenshots*))
  "Returns the type of the screenshot specified by handle."
  (cffi:with-foreign-object (screenshot-error 'vr-screenshot-error)
    (prog1 (%get-screenshot-property-type (table screenshots) handle screenshot-error)
      (unless (eq :none (cffi:mem-ref screenshot-error 'vr-screenshot-error))
        (error "VR screenshot error ~a" (cffi:mem-ref screenshot-error 'vr-screenshot-error)))))) ; works


(defun screenshot-property-filename (handle filename-type &key (screenshots *screenshots*))
  "Get the filename associated with a screenshot handle. filename-type must be :PREVIEW or :VR"
  (cffi:with-foreign-string (buffer (make-string 512 :initial-element #\space))
    (cffi:with-foreign-object (error-pointer 'vr-screenshot-error) 
      (%get-screenshot-property-filename (table screenshots) handle filename-type buffer 512
                                         error-pointer)
      (unless (eq :none (cffi:mem-ref error-pointer 'vr-screenshot-error))
        (error "VR screenshot error ~a" (cffi:mem-ref error-pointer 'vr-screenshot-error)))
      (cffi:foreign-string-to-lisp buffer)))) ; works


(defun update-screenshot-progress (screenshot progress &key (screenshots *screenshots*))
  "Present the user with a progress display during screenshot generation."
  (let ((error-value (%update-screenshot-progress (table screenshots) (handle screenshot) progress)))
    (unless (eq error-value :none)
      (error "VR screenshot error ~a" error-value)))) ; works?


(defun take-stereo-screenshot (preview-pathname vr-pathname &key (screenshots *screenshots*))
  "Request a stereoscopic screenshot."
  (cffi:with-foreign-object (handle '(:pointer screenshot-handle-t))
    (let ((error-value
            (%take-stereo-screenshot (table screenshots)
                                     handle (namestring preview-pathname)
                                     (namestring vr-pathname))))
      (unless (eq error-value :none) (error "VR screenshot error ~a" error-value))
      (screenshot-from-handle handle)))) ; works?


(defun submit-screenshot (screenshot &key (screenshots *screenshots*))
  "Submit a new screenshot to the Steam API."
  (let ((error-value
          (%submit-screenshot (table screenshots) (handle screenshot) (screenshot-type screenshot)
                              (namestring (preview-pathname screenshot))
                              (namestring (vr-pathname screenshot)))))
    (unless (eq error-value :none) (error "VR screenshot error ~a" error-value)))) ; works?

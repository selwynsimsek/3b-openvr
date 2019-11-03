;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High level bindings for the IVRRenderModels interface.
;;; https://github.com/ValveSoftware/openvr/wiki/IVRRenderModels_Overview

(in-package 3b-openvr)

(defclass render-model ()
  ((name :initarg :name :accessor name)
   (vertices :initarg :vertices :accessor vertices)
   (indices :initarg :indices :accessor indices)
   (diffuse-texture :initarg :diffuse-texture :accessor diffuse-texture)
   (loaded-p :initarg :loaded-p :accessor loaded-p)
   (foreign-pointer :initarg :foreign-pointer :accessor foreign-pointer)
   (components :initarg :components :accessor components)))

(defclass texture-map ()
  ((handle :initarg :handle :accessor handle)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (data :initarg :data :accessor data)
   (loaded-p :initarg :loaded-p :accessor loaded-p)
   (foreign-pointer :initarg :foreign-pointer :accessor foreign-pointer)))

(defclass component ()
  ((button-mask :initarg :button-mask :accessor button-mask)
   (name :initarg :name :accessor name)
   (render-model-name :initarg :render-model-name :accessor render-model-name)
   (state :initarg :state :accessor state)))

(defun load-render-model (name &key (render-models *render-models*))
  "Loads and returns a render model for use in the application. name should be a 
   render model name from the Prop_RenderModelName_String property or an absolute path name to a
   render model on disk."
  (error "implement me"))

(defun wait-until-loaded (render-model &key (render-models *render-models*))
  "Blocks until the render model is loaded or until an error occurs."
  (error "implement me"))

(defun try-to-load-model (render-model &key (render-models *render-models*))
  (error "implement me"))

(defun try-to-load-texture-map (texture-map &key (render-models *render-models*))
  (error "implement me"))

(defun render-model-names (render-model-index &key (render-models *render-models*))
  "Returns an array of available render models. The index used in the array does not necessarily
   correspond to a tracked device index."
  (let ((names (make-array (list (%get-render-model-count (table render-models))))))
    (loop for i below (length names)
          do (cffi:with-foreign-string (foreign-string (make-string 512))
               (%get-render-model-name (table render-models) i foreign-string 512)
               (setf (aref names i) (cffi:foreign-string-to-lisp foreign-string)))
          finally (return names))))

(defun update-component-states-for-device (render-model
                                           input-device
                                           &key (render-models *render-models*)
                                                (scroll-wheel-visible-p nil))
  "Updates the component states of the render model given the specified input device.
   For dynamic controller components (ex: trigger) values will reflect component motions."
  (error "implement me"))

(defun thumbnail-url (render-model &key (render-models *render-models*))
  "Returns the URL of the thumbnail image for render-model."
  (cffi:with-foreign-string (foreign-string (make-string 512))
    (cffi:with-foreign-object (error-pointer 'vr-render-model-error)
      (%get-render-model-thumbnail-url 
       (table render-models)
       (name render-model)
       foreign-string
       512
       error-pointer)
      (if (eq error-pointer :none)
          (cffi:foreign-string-to-lisp foreign-string)
          (error "Render model error: ~a" (cffi:mem-ref error-pointer 'vr-render-model-error))))))

(defun original-render-model (render-model &key (render-models *render-models*))
  "Provides the unskinned model if the name of render-model has been replaced by the user. If the 
   name hasn't been replaced the returned render-model will still be valid."
  (cffi:with-foreign-string (foreign-string (make-string 512))
    (cffi:with-foreign-object (error-pointer 'vr-render-model-error)
      (%get-render-model-original-path
       (table render-models)
       (name render-model)
       foreign-string
       512
       error-pointer)
      (if (eq error-pointer :none)
          (load-render-model (cffi:foreign-string-to-lisp foreign-string))
          (error "Render model error: ~a" (cffi:mem-ref error-pointer 'vr-render-model-error))))))

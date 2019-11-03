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
  (let ((render-model (make-instance 'render-model :name name)))
    (try-to-load-model render-model :render-models *render-models*)
    render-model))

(defun wait-until-loaded (render-model &key (render-models *render-models*) (sleep-time 0.1))
  "Blocks until the render model is loaded or until an error occurs."
  (loop while (not (try-to-load-model render-model :render-models render-models))
        do (sleep sleep-time)))

(defun try-to-load-model (render-model &key (render-models *render-models*))
  "Tries to load the model asynchronously. Returns T on success and NIL if still loading."
  (with-slots (name vertices indices diffuse-texture loaded-p foreign-pointer components)
      render-model
    (when loaded-p (return t))
    (unless foreign-pointer
      (setf foreign-pointer (cffi:foreign-alloc '(:pointer (:struct render-model-t)))))
    (let ((error-value
            (%load-render-model-async (table render-models) name foreign-pointer)))
      (when (eq error-value :loading)
        (return nil))
      (unless (eq error-value :none)
        (error "Render models error: ~a" error-value))
                                        ; continue loading
      (cffi:with-foreign-slots ((vertex-data vertex-count index-data triangle-count
                                             diffuse-texture-id)
                                (cffi:mem-ref foreign-pointer :pointer)
                                '(:struct render-model-t))
        (setf vertices (make-array (list vertex-count)))
        (setf indices (make-array (list (* 3 triangle-count))))
        (setf diffuse-texture (make-instance 'texture-map :handle diffuse-texture-id))
        (loop for i from 0 below (length indices)
              do (setf (aref indices i) (cffi:mem-ref index-data :uint16 i)))
        (loop for i from 0 below (length vertices)
              do (setf (aref vertices i) (cffi:mem-ref vertex-data '(:struct render-model-vertex-t)
                                                       i)))
        (setf loaded-p (try-to-load-texture-map diffuse-texture))
        (when loaded-p
          (%free-render-model (table render-models)
                              (cffi:mem-ref foreign-pointer :pointer))
          (cffi:foreign-free foreign-pointer))
        loaded-p))))

(defun try-to-load-texture-map (texture-map &key (render-models *render-models*))
  "Loads the texture map asynchronously. Returns T on success and NIL if still loading."
  (with-slots (handle (texture-width width) (texture-height height) data loaded-p foreign-pointer) texture-map
    (when loaded-p (return t))
    (unless foreign-pointer
      (setf foreign-pointer
            (cffi:foreign-alloc '(:pointer (:struct render-model-texture-map-t)))))
    (let ((error-value (%load-texture-async (table render-models) handle foreign-pointer)))
      (when (eq error-value :loading) (return nil))
      (unless (eq error-value :none)
        (error "Render models error: ~a" error-value))
      (cffi:with-foreign-slots ((width height texture-map-data) (cffi:mem-ref foreign-pointer :pointer) '(:struct render-model-texture-map-t))
        (setf texture-width width)
        (setf texture-height height)
        (setf data (make-array (list (* 4 width height))))
        (loop for i from 0 below (length data)
              do (setf (aref data i) (cffi:mem-aref texture-map-data :uint8 i)))
        (setf loaded-p t)
        (%free-texture (table render-models) (cffi:mem-ref foreign-pointer :pointer))
        (cffi:foreign-free foreign-pointer)
        t))))

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

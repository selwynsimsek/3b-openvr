;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High level bindings for the IVRRenderModels interface.
;;; https://github.com/ValveSoftware/openvr/wiki/IVRRenderModels_Overview

(in-package 3b-openvr)

(defclass render-model-vertex ()
  ((location :initarg :position :accessor location)
   (normal :initarg :normal :accessor normal)
   (texture-coordinate :initarg :texture-coordinate :accessor texture-coordinate)))

(defclass render-model-texture-map ()
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (data :initarg :data :accessor data)))

(defclass render-model ()
  ((internal-handle :initarg :internal-handle :accessor internal-handle)
   (vertex-data :initarg :vertex-data :accessor vertex-data)
   (index-data :initarg :index-data :accessor index-data)
   (diffuse-texture :initarg :diffuse-texture :accessor diffuse-texture)))

;; (defun async-render-model (name &key (render-models *render-models*))
;;   "Loads and returns a render model for use in the application."
;;   (let ((render-model (make-instance 'render-model)))
;;     (cffi:with-foreign-object (model-pointer '(:pointer vr-render-model-t-tclass))
;;       (let ((status (%load-render-model-async (table render-models) name model-pointer)))
;;         (when (eq status :loading) (return :loading))
;;         (unless (eq status :none) (error "VR render model error status: ~a" status)))
;;       ; fill in slots here
;;       (%free-render-model (table render-models) model-pointer))
;;     render-model))

(defun render-model-name (index &key (render-models *render-models*))
  "Returns the name of the Nth render model that is provided by the OpenVR runtime."
  (cffi:with-foreign-string (buffer (make-string 512 :initial-element #\space))
    (let ((return-val (%get-render-model-name (table render-models) index buffer 512)))
      (unless (zerop return-val)
        (cffi:foreign-string-to-lisp buffer)))))

(defun render-model-count (&key (render-models *render-models*))
  "Returns the number of render models provided by the OpenVR runtime."
  (%get-render-model-count (table render-models)))


(defun load-render-model-async (name &key (render-models *render-models*))
  (cffi:with-foreign-object (prm '(:pointer (:struct render-model-t)))
    (let ((r (check-ret
              (%load-render-model-async (table render-models) name prm)
              :ok (:none :loading))))
      ;; return NIL to indicate :loading for now. possibly should
      ;; error w/restarts, but that sounds annoying for general case,
      ;; since :loading is probably most common return value.
      (when (eql r :none)
        (cffi:mem-ref
         (cffi:mem-ref prm '(:pointer (:struct render-model-t)))
         '(:struct render-model-t))))))

(defun load-texture-async (name &key (render-models *render-models*))
  (cffi:with-foreign-object (prmt '(:pointer
                                    (:struct render-model-texture-map-t)))
    (let ((r (check-ret
              (%load-texture-async (table *render-models*) name prmt)
              :ok (:none :loading))))
      ;; return NIL for :loading
      (when (eql r :none)
        (cffi:mem-ref
         (cffi:mem-ref prmt '(:pointer (:struct render-model-texture-map-t)))
         '(:struct render-model-texture-map-t))))))

(defun free-render-model (model &key (render-models *render-models*))
  (cffi:with-foreign-object (prm '(:struct render-model-t))
    (setf (cffi:mem-ref prm '(:struct render-model-t))
          model)
    (%free-render-model (table render-models) prm)))

(defun free-texture (texture &key (render-models *render-models*))
  (cffi:with-foreign-object (prmt '(:struct render-model-texture-map-t))
    (setf (cffi:mem-ref prmt '(:struct render-model-texture-map-t))
          texture)
    (%free-texture (table render-models) prmt)))

(defun get-render-model-count (&key (render-models *render-models*))
  (%get-render-model-count (table render-models)))

(defun get-render-model-name (index &key (render-models *render-models*))
  (let ((l (%get-render-model-name (table render-models) index
                                   (null-pointer) 0)))
    (when (plusp l)
      (cffi:with-foreign-pointer-as-string (p l)
        (%get-render-model-name (table render-models) index p l)))))

(defun get-render-model-names (&key (render-models *render-models*))
  (let ((c (%get-render-model-count (table render-models))))
    (loop for i below c
          collect (get-render-model-name i :render-models render-models))))

(defun get-component-count (render-model-name
                            &key (render-models *render-models*))
  (%get-component-count (table render-models) render-model-name))

(defun get-component-name (index render-model-name
                           &key (render-models *render-models*))
  (let ((l (%get-component-name (table render-models)
                                render-model-name index
                                (null-pointer) 0)))
    (when (plusp l)
      (cffi:with-foreign-pointer-as-string (p l)
        (%get-component-name (table render-models)
                             render-model-name index p l)))))

(defun get-component-render-model-name (render-model-name component-name
                                        &key (render-models *render-models*))
  (let ((l (%get-component-render-model-name (table render-models)
                                             render-model-name component-name
                                             (null-pointer) 0)))
    (when (plusp l)
      (cffi:with-foreign-pointer-as-string (p l)
        (%get-component-render-model-name (table render-models)
                                          render-model-name component-name p l)))))

(defun get-component-names (render-model-name
                            &key (render-models *render-models*))
  (let ((c (%get-component-count (table render-models) render-model-name)))
    (loop for i below c
          collect (get-component-name i render-model-name
                                      :render-models render-models))))

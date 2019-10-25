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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the IVRSpatialAnchors API.

;;; IVR_SpatialAnchors_001

(in-package 3b-openvr)



(defmacro with-spatial-anchor-error (&rest body)
  (let ((error-name (gensym "error-value")))
    `(let ((,error-name (progn ,@body)))
       (if (eq ,error-name :success) t (error "VR overlay error: ~a" ,error-name)))))


(defclass spatial-anchor ()
  ((handle :initarg :handle :accessor handle)))


(defun spatial-anchor-from-descriptor (descriptor &key (spatial-anchors *spatial-anchors*))
  "Returns a handle for an spatial anchor described by descriptor.
   Caller can wait for an event or occasionally poll spatial-anchor-pose to find the virtual 
   coordinate associated with this anchor."
  (cffi:with-foreign-object (pointer 'spatial-anchor-handle-t)
    (with-spatial-anchor-error
        (%create-spatial-anchor-from-descriptor (table spatial-anchors) descriptor pointer))
    (make-instance 'spatial-anchor :handle (cffi:mem-ref pointer 'spatial-anchor-handle-t))))


(defun spatial-anchor-from-pose (device-index tracking-universe-origin pose
                                        &key (spatial-anchors *spatial-anchors*))
  "Returns a handle for an new spatial anchor at pPose. Caller can wait for an event or occasionally
  poll #'spatial-anchor-descriptor to find the permanent descriptor for this pose. The result of 
  #'spatial-anchor-pose may evolve from this initial position if the driver chooses to update it.
  The anchor will be associated with the driver that provides device-index, and the driver may use 
  that specific device as a hint for how to best create the anchor.
  tracking-universe-origin must match whatever tracking origin you are working in 
  (seated/standing/raw). This should be called when the user is close to (and ideally looking 
  at/interacting with) the target physical location.  At that moment, the driver will have the most 
  information about how to recover that physical point in the future, and the quality of the anchor
  (when the descriptor is re-used) will be highest. The caller may decide to apply offsets from this
  initial pose, but is advised to stay relatively close to the original pose location for highest 
  fidelity."
  (cffi:with-foreign-object (pointer 'spatial-anchor-handle-t)
    (with-spatial-anchor-error
        (%create-spatial-anchor-from-pose (table spatial-anchors) device-index tracking-universe-origin
                                          pose pointer)) ; need a type translator here? check this
    (make-instance 'spatial-anchor :handle (cffi:mem-ref pointer 'spatial-anchor-handle-t))))


(defun spatial-anchor-pose (spatial-anchor tracking-universe-origin
                            &key (spatial-anchors *spatial-anchors*))
  "Get the pose for a given handle.  This is intended to be cheap enough to call every frame 
  (or fairly often) so that the driver can refine this position when it has more information 
  available."
  (cffi:with-foreign-object (pointer '(:struct spatial-anchor-pose-t))
    (with-spatial-anchor-error
        (%get-spatial-anchor-pose (table spatial-anchors) (handle spatial-anchor)
                                  tracking-universe-origin pointer))
    (cffi:mem-ref pointer '(:struct spatial-anchor-pose-t))))


(defun spatial-anchor-descriptor (spatial-anchor &key (spatial-anchors *spatial-anchors*))
  "Get the descriptor for a given handle.  This will be empty for handles where the driver has not
  yet built a descriptor.  It will be the application-supplied descriptor for previously saved 
  anchors that the application is requesting poses for.  If the driver has called 
  UpdateSpatialAnchorDescriptor() already in this session, it will be the descriptor provided by 
  the driver."
  (cffi:with-foreign-string (pointer (make-string 512))
    (with-spatial-anchor-error
        (%get-spatial-anchor-descriptor (table spatial-anchors)
                                        (handle spatial-anchor)
                                        pointer 512))
    (cffi:foreign-string-to-lisp pointer)))

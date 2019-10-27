;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the IVRCompositor API.

;;;IVR_Compositor_022

(in-package 3b-openvr)

(defun submit (eye texture &key (compositor *compositor*)
                                bounds (flags :default))
  (cffi:with-foreign-objects ((ptexture '(:struct texture-t))
                              (pbounds '(:struct vr-texture-bounds-t)))
    (when (numberp (getf texture 'handle))
      (setf (getf texture 'handle) (cffi:make-pointer (getf texture 'handle))))
    (setf (cffi:mem-ref ptexture '(:struct texture-t)) texture)
    (when bounds
      (setf (cffi:mem-ref pbounds '(:struct vr-texture-bounds-t)) bounds))
    (%submit (table compositor) eye ptexture (if bounds
                                                 pbounds
                                                 (cffi:null-pointer))
             (alexandria:ensure-list flags))))

(defun wait-get-poses  (pose-array game-pose-array
                        &key (compositor *compositor*))
  (cffi:with-foreign-objects ((ppa '(:struct tracked-device-pose-t)
                                   (length pose-array))
                              (pgpa '(:struct tracked-device-pose-t)
                                    (length game-pose-array)))
    (check-ret
     (%wait-get-poses (table compositor)
                      (if pose-array ppa (null-pointer))
                      (length pose-array)
                      (if game-pose-array pgpa (null-pointer))
                      (length game-pose-array)))
    (loop for i below (length pose-array)
          for p = (cffi:mem-aptr ppa '(:struct tracked-device-pose-t) i)
          do (setf (aref pose-array i)
                   (if (cffi:foreign-slot-value p
                                                '(:struct tracked-device-pose-t)
                                                'pose-is-valid)
                       (cffi:mem-ref p '(:struct tracked-device-pose-t))
                       (list 'pose-is-valid nil))))

    (loop for i below (length game-pose-array)
          for p = (cffi:mem-aptr pgpa '(:struct tracked-device-pose-t) i)
          do (setf (aref game-pose-array i)
                   (if (cffi:foreign-slot-value p
                                                '(:struct tracked-device-pose-t)
                                                'pose-is-valid)
                       (cffi:mem-ref p '(:struct tracked-device-pose-t))
                       (list 'pose-is-valid nil))))))

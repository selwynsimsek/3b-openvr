;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level API for the IVRExtendedDisplay API.

;;; IVR_ExtendedDisplay_001

;; NOTE: Use of this interface is not recommended in production applications.
;; It will not work for displays which use direct-to-display mode.
;; Creating our own window is also incompatible with the VR compositor
;; and is not available when the compositor is running.

(defun window-bounds (&key (extended-display *extended-display*))
  "Size and position that the window needs to be on the VR display."
  (cffi:with-foreign-objects
      ((x '(:pointer :int32))
       (y '(:pointer :int32))
       (width '(:pointer :uint32))
       (height '(:pointer :uint32)))
    (%get-window-bounds (table extended-display) x y width height)
    (values (cffi:mem-ref x :int32) (cffi:mem-ref y :int32)
            (cffi:mem-ref width :uint32) (cffi:mem-ref height :uint32))))

(defun eye-output-viewport (eye &key (extended-display *extended-display*))
  "Gets the viewport in the frame buffer to draw the output of the distortion into."
  (cffi:with-foreign-objects
      ((x '(:pointer :int32))
       (y '(:pointer :int32))
       (width '(:pointer :uint32))
       (height '(:pointer :uint32)))
    (%get-eye-output-viewport (table extended-display) eye x y width height)
    (values (cffi:mem-ref x :int32) (cffi:mem-ref y :int32)
            (cffi:mem-ref width :uint32) (cffi:mem-ref height :uint32))))

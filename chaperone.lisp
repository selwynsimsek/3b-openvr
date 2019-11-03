;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the Chaperone API.

;;; IVR_Chaperone_003

(in-package :3b-openvr)

(defun calibration-state (&key (chaperone *chaperone*))
  "Get the current state of Chaperone calibration. This state can change at any time during a 
   session due to physical base station changes."
  (%get-calibration-state (table chaperone)))

(defun play-area-size (&key (chaperone *chaperone*))
  "Returns the width and depth of the Play Area."
  (cffi:with-foreign-objects ((x :float)
                              (z :float))
    (%get-play-area-size (table chaperone) x z)
    (values (cffi:mem-ref x :float) (cffi:mem-ref z :float))))

(defun play-area-rect (&key (chaperone *chaperone*))
  "Returns the 4 corner positions of the Play Area."
  (cffi:with-foreign-object (pointer '(:struct hmd-quad-t))
    (%get-play-area-rect (table chaperone) pointer)
    (cffi:mem-ref pointer '(:struct hmd-quad-t))))

(defun reload-chaperone-info (&key (chaperone *chaperone*))
  "Reload Chaperone data from the .vrchap file on disk."
  (%reload-info (table chaperone)))

(defun set-scene-color (color &key (chaperone *chaperone*))
  "Optionally give the chaperone system a hint about the color and brightness in the scene."
  (cffi:with-foreign-object (color-pointer '(:struct hmd-color-t))
    (setf (cffi:mem-ref color-pointer '(:struct hmd-color-t)) color)
    (%set-scene-color (table chaperone) color-pointer)))

(defun bounds-color (collision-bounds-fade-distance &key (chaperone *chaperone*))
  "Get the current chaperone bounds draw color and brightness."
  (error "implement me")) ; not sure what to do here?

(defun bounds-visible-p (&key (chaperone *chaperone*))
  "Determine whether the bounds are showing right now."
  (%are-bounds-visible (table chaperone)))

(defun force-bounds-visible (bounds-visible-p &key (chaperone *chaperone*))
  "Force the bounds to show, mostly for utilities."
  (%force-bounds-visible (table chaperone) bounds-visible-p))

(export '(calibration-state play-area-size play-area-rect reload-chaperone-info set-scene-color
          bounds-color bounds-visible-p force-bounds-visible))

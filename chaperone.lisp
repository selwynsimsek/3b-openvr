;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the Chaperone API.

;;; IVR_Chaperone_003

(in-package :3b-openvr)

(annot:enable-annot-syntax)

@export
(defun calibration-state (&key (chaperone *chaperone*))
  "Get the current state of Chaperone calibration. This state can change at any time during a 
   session due to physical base station changes."
  (%get-calibration-state (table chaperone))) ; works

@export
(defun play-area-size (&key (chaperone *chaperone*))
  "Returns the width and depth of the Play Area."
  (cffi:with-foreign-objects ((x :float)
                              (z :float))
    (%get-play-area-size (table chaperone) x z)
    (values (cffi:mem-ref x :float) (cffi:mem-ref z :float)))) ; works

@export
(defun play-area-rect (&key (chaperone *chaperone*))
  "Returns the 4 corner positions of the Play Area."
  (cffi:with-foreign-object (pointer '(:struct hmd-quad-t))
    (%get-play-area-rect (table chaperone) pointer)
    (cffi:mem-ref pointer '(:struct hmd-quad-t)))) ; works

@export
(defun reload-chaperone-info (&key (chaperone *chaperone*))
  "Reload Chaperone data from the .vrchap file on disk."
  (%reload-info (table chaperone))) ; apparently works
(define-clos-wrapper (color hmd-color-t) ()
                     ((r r)
                      (g g)
                      (b b)
                      (a a)))
@export
(defun set-scene-color (color &key (chaperone *chaperone*))
  "Optionally give the chaperone system a hint about the color and brightness in the scene."
  (cffi:with-foreign-object (color-pointer '(:struct hmd-color-t))
    (cffi:with-foreign-slots ((r g b a) color-pointer (:struct hmd-color-t))
      (setf r (r color)
            g (g color)
            b (b color)
            a (a color))
      (%set-scene-color (table chaperone) color)))) ; doesn't really work

(defun bounds-color (collision-bounds-fade-distance &key (chaperone *chaperone*))
  "Get the current chaperone bounds draw color and brightness."
  (cffi:with-foreign-objects ((color-array '(:struct hmd-color-t) 10)
                              (camera-color '(:struct hmd-color-t)))
    (%get-bounds-color (table chaperone) color-array 10 collision-bounds-fade-distance
                       camera-color)
    (let ((return-vector (make-array (list 10))))
      (loop for i from 0 below (length return-vector) do
            (setf (aref return-vector i) (cffi:mem-aref color-array '(:struct hmd-color-t) i)))
      (values (cffi:mem-aref camera-color '(:struct hmd-color-t)) return-vector)))) ; doesn't really work

(defmethod print-object ((object color) stream)
  (print-unreadable-object (object stream)
    (format stream "r ~a g ~a b ~a a ~a" (r object) (g object) (b object) (a object))))

@export
(defun bounds-visible-p (&key (chaperone *chaperone*))
  "Determine whether the bounds are showing right now."
  (%are-bounds-visible (table chaperone))) ; works

@export
(defun force-bounds-visible (bounds-visible-p &key (chaperone *chaperone*))
  "Force the bounds to show, mostly for utilities."
  (%force-bounds-visible (table chaperone) bounds-visible-p)) ; works

(export '(r g a color))

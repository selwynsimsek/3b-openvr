;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; A CLOS interface to the Overlay API.

(in-package :3b-openvr)

(defun overlay-key ()
  "Returns unique overlay keys each time. Not a good idea to rebind *gensym-counter* between calls to this."
  (symbol-name (gensym "3B-OPENVR-OVERLAY")))

(defclass overlay ()
  ((key :accessor key)
   (handle :accessor handle :initform 0)
   (name :reader name :initarg :name :initform "")))

(defmethod initialize-instance :after ((instance overlay) &key)
  (setf (key instance) (overlay-key))
  (when *overlay*
    (setf (handle instance) (create-overlay (key instance) (name instance)))))

(defun (setf name) (name overlay)
  (set-overlay-name (handle overlay) name)
  (setf (slot-value overlay 'name) name)) ; appears to work

(defmethod image-data ((overlay overlay)) (overlay-image-data (handle overlay))) ; appears to work

(defun (setf rendering-pid) (pid overlay) (set-overlay-rendering-pid (handle overlay) pid)) ; works

(defmethod rendering-pid ((overlay overlay)) (get-overlay-rendering-pid (handle overlay))) ; works

(defmethod flag (flag (overlay overlay)) (get-overlay-flag (handle overlay) flag)) ; works

(defun (setf flag) (enabled-p flag overlay) (set-overlay-flag (handle overlay) flag enabled-p))
                                        ; works

(defmethod flags ((overlay overlay)) (overlay-flags (handle overlay))) ; works

(defmethod color ((overlay overlay)) (overlay-color (handle overlay))) ; works

(defun (setf color) (color overlay)
  (set-overlay-color (handle overlay) (aref color 0) (aref color 1) (aref color 2))) ; works

(defmethod alpha ((overlay overlay)) (overlay-alpha (handle overlay))) ; works

(defun (setf alpha) (alpha overlay) (set-overlay-alpha (handle overlay) alpha)) ; works

(defmethod texel-aspect ((overlay overlay)) (overlay-texel-aspect (handle overlay))) ; works

(defun (setf texel-aspect) (texel-aspect overlay)
  (set-overlay-texel-aspect (handle overlay) texel-aspect)) ; works

(defmethod sort-order ((overlay overlay)) (overlay-sort-order (handle overlay)))

(defun (setf sort-order) (sort-order overlay) (set-overlay-sort-order (handle overlay) sort-order))

(defmethod width-in-meters ((overlay overlay)) (overlay-width-in-meters (handle overlay)))

(defun (setf width-in-meters) (width-in-meters overlay)
  (set-overlay-width-in-meters (handle overlay) width-in-meters))

(defmethod curvature ((overlay overlay)) (overlay-curvature (handle overlay)))

(defun (setf curvature) (curvature overlay) (set-overlay-curvature (handle overlay) curvature))

(defmethod texture-color-space ((overlay overlay)) (overlay-texture-color-space (handle overlay)))

(defun (setf texture-color-space) (curvature overlay)
  (set-overlay-texture-color-space (handle overlay) curvature))

(defmethod texture-bounds ((overlay overlay)) (overlay-texture-bounds (handle overlay)))

(defun (setf texture-bounds) (bounds overlay)
  (set-overlay-texture-bounds (handle overlay)
                              (coerce (aref bounds 0) 'single-float)
                              (coerce (aref bounds 1) 'single-float)
                              (coerce (aref bounds 2) 'single-float)
                              (coerce (aref bounds 3) 'single-float)))

(defmethod transform-type ((overlay overlay)) (overlay-transform-type (handle overlay)))

(defmethod transform ((overlay overlay) &key (origin :standing))
  (let ((type (transform-type overlay)))
    (cond ((eq :absolute type)
           (values type (overlay-transform-absolute (handle overlay) origin)))
          ((eq :tracked-device-relative type)
           (apply #'values
                  (cons type
                        (multiple-value-list (overlay-transform-tracked-device-relative (handle overlay))))))
          ((eq :tracked-component type)
           (apply #'values
                  (cons type
                        (multiple-value-list (overlay-transform-tracked-device-component (handle overlay))))))
          ((eq :cursor type)
           (values type (overlay-transform-cursor (handle overlay)))))))

(defun (setf transform) (transform overlay type &key (origin :standing) (tracked-device 0)
                                                     (device-index 0) (component-name))
  (cond ((eq :absolute type)
         (set-overlay-transform-absolute (handle overlay) origin transform))
        ((eq :tracked-device-relative type)
         (set-overlay-transform-tracked-device-relative
          (handle overlay) tracked-device transform))
        ((eq :tracked-component type)
         (set-overlay-transform-tracked-device-component (handle overlay) device-index
                                                         transform))
        ((eq :cursor type)
         (set-overlay-transform-cursor (handle overlay) transform))))

(defun (setf location) (position overlay)
  (set-overlay-transform-absolute
   (handle overlay) :standing (vector 1.0 0.0 0.0 (aref position 0)
                                      0.0 1.0 0.0 (aref position 1)
                                      0.0 0.0 1.0 (aref position 2))))

(defmethod show ((overlay overlay)) (show-overlay (handle overlay)))
(defmethod hide ((overlay overlay)) (hide-overlay (handle overlay)))
(defmethod visible-p ((overlay overlay)) (overlay-visible-p (handle overlay)))


(defmethod transform-for-coordinates ((overlay overlay) tracking-origin coordinates)
  (transform-for-overlay-coordinates (handle overlay) tracking-origin coordinates))

(defmethod poll ((overlay overlay)) (poll-next-overlay-event (handle overlay)))

(defmethod input-method ((overlay overlay)) (overlay-input-method (handle overlay)))

(defun (setf input-method) (input-method overlay)
  (set-overlay-input-method (handle overlay) input-method))

(defun (setf mouse-scale) (mouse-scale overlay)
  (set-overlay-mouse-scale (handle overlay) mouse-scale))

(defmethod mouse-scale ((overlay overlay)) (overlay-mouse-scale (handle overlay)))

(defmethod compute-intersection ((overlay overlay) source direction origin)
  (compute-overlay-intersection (handle overlay) source direction origin))

(defmethod hover-target-p ((overlay overlay)) (hover-target-overlay-p (handle overlay)))

(defmethod set-intersection-mask ((overlay overlay) mask-primitives)
  (set-overlay-intersection-mask (handle overlay)))

(defmethod trigger-laser-mouse-haptic-vibration ((overlay overlay) duration-in-seconds frequency
                                                 amplitude)
  (do-trigger-laser-mouse-haptic-vibration (handle overlay) duration-in-seconds frequency
    amplitude))

(defun (setf cursor) (cursor overlay)
  (set-overlay-cursor (handle overlay) cursor))

(defun (setf cursor-position-override) (cursor overlay)
  "Sets the override cursor position to use for this overlay in overlay mouse coordinates.
   If cursor is NIL, then the override cursor position is cleared for this overlay."
  (if cursor
      (set-overlay-cursor-position-override (handle overlay) cursor)
      (clear-overlay-cursor-position-override (handle overlay))))

(defun (setf texture) (texture overlay)
  "Texture to draw for the overlay. If texture is NIL then the overlay system is told to release
the texture set for this overlay."
  (if texture
      (set-overlay-texture (handle overlay) texture)
      (clear-overlay-texture (handle overlay))))

(defun (setf raw) (buffer overlay width height bytes-per-pixel)
  (set-overlay-raw (handle overlay) buffer width height bytes-per-pixel))

(defun (setf from-file) (file-path overlay)
  (set-overlay-from-file overlay file-path))

(defmethod texture-size ((overlay overlay))
  (overlay-texture-size (handle overlay)))

(defclass dashboard-overlay (overlay)
  ((thumbnail-handle :accessor handle :initform 0)))

(defmethod initialize-instance :after ((instance dashboard-overlay) &key)
  (when *overlay*
    (multiple-value-bind (main thumbnail)
        (create-dashboard-overlay (key instance) (name instance))
      (setf (handle instance) main
            (thumbnail-handle instance) thumbnail))))

(defmethod active-p ((overlay dashboard-overlay))
  (dashboard-overlay-active-p (handle overlay)))

(defun (setf scene-process) (process-id overlay)
  (set-dashboard-overlay-scene-process (handle overlay) process-id))

(defmethod scene-process ((overlay dashboard-overlay))
  (dashboard-overlay-scene-process (handle overlay)))

(defmethod keyboard-show ((overlay overlay) input-mode line-input-mode description max-char
                          existing-text use-minimal-mode-p user-value)
  (show-keyboard-for-overlay (handle overlay) input-mode line-input-mode description max-char
                             existing-text use-minimal-mode-p user-value))


(defun (setf keyboard-position) (avoid-rect overlay)
  (set-keyboard-position-for-overlay (handle overlay) avoid-rect))

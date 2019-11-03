;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the ChaperoneSetup API.

;;; IVR_ChaperoneSetup_006

(in-package :3b-openvr)

(defun commit-working-copy (chaperone-configuration-file &key (chaperone-setup *chaperone-setup*))
  (%commit-working-copy (table chaperone-setup) chaperone-configuration-file))

(defun revert-working-copy (&key (chaperone-setup *chaperone-setup*))
  (%revert-working-copy (table chaperone-setup)))

(defun working-play-area-size (&key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-objects ((x :float)
                              (z :float))
    (%get-working-play-area-size (table chaperone-setup) x z)
    (values (cffi:mem-ref x :float) (cffi:mem-ref z :float))))

(defun working-play-area-rect (&key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (pointer '(:struct hmd-quad-t))
    (%get-working-play-area-rect (table chaperone-setup) pointer)
    (cffi:mem-ref pointer '(:struct hmd-quad-t))))

(defun working-collision-bounds-info (&key (chaperone-setup *chaperone-setup*))
  (error "implement me"))

(defun live-collision-bounds-info (&key (chaperone-setup *chaperone-setup*))
  (error "implement me"))

(defun working-seated-zero-pose-to-raw-tracking-pose (&key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (pointer '(:struct hmd-matrix-34-t))
    (%get-working-seated-zero-pose-to-raw-tracking-pose
     (table chaperone-setup) pointer)
    (cffi:mem-ref pointer '(:struct hmd-matrix-34-t))))

(defun working-standing-zero-pose-to-raw-tracking-pose (&key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (pointer '(:struct hmd-matrix-34-t))
    (%get-working-standing-zero-pose-to-raw-tracking-pose
     (table chaperone-setup) pointer)
    (cffi:mem-ref pointer '(:struct hmd-matrix-34-t))))

(defun set-working-play-area-size (x z &key (chaperone-setup *chaperone-setup*))
  (%set-working-play-area-size (table chaperone-setup) x z))

(defun set-working-collision-bounds-info (quads &key (chaperone-setup *chaperone-setup*))
  (error "implement me"))

(defun set-working-perimeter (points &key (chaperone-setup *chaperone-setup*))
  (error "implement me"))

(defun set-working-seating-zero-pose-to-raw-tracking-pose (pose &key (chaperone-setup *chaperone-setup*))
  (error "implement me"))

(defun set-working-standing-zero-pose-to-raw-tracking-pose (pose &key (chaperone-setup *chaperone-setup*))
  (error "implement me"))

(defun reload-from-disk (configuration-file &key (chaperone-setup *chaperone-setup*))
  (%reload-from-disk (table chaperone-setup) configuration-file))

(defun live-seated-zero-pose-to-raw-tracking-pose (&key (chaperone-setup *chaperone-setup*))
  (error "implement me"))

(defun export-live-to-buffer (&key (chaperone-setup *chaperone-setup*))
  (error "implement me"))

(defun import-from-buffer-to-working (&key (chaperone-setup *chaperone-setup*))
  (error "implement me"))

(defun show-working-set-preview (&key (chaperone-setup *chaperone-setup*))
  "Shows the chaperone data in the working set to preview in the compositor."
  (%show-working-set-preview (table chaperone-setup)))

(defun hide-working-set-preview (&key (chaperone-setup *chaperone-setup*))
  "Hides the chaperone data in the working set to preview in the compositor (if it was visible)."
  (%hide-working-set-preview (table chaperone-setup)))

(defun room-setup-starting (&key (chaperone-setup *chaperone-setup*))
  "Fire an event that the tracking system can use to know room setup is about to begin. This lets the tracking system make any last minute adjustments that should be incorporated into the new setup.  If the user is adjusting live in HMD using a tweak tool, keep in mind that calling this might cause the user to see the room jump."
  (%room-setup-starting (table chaperone-setup)))

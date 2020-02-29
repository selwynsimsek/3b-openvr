;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the ChaperoneSetup API.

;;; IVR_ChaperoneSetup_006

(in-package :3b-openvr)

(annot:enable-annot-syntax)

@export
(defun commit-working-copy (chaperone-configuration-file &key (chaperone-setup *chaperone-setup*))
  "Saves the current working copy to disk."
  (%commit-working-copy (table chaperone-setup) chaperone-configuration-file)) ; works

@export
(defun revert-working-copy (&key (chaperone-setup *chaperone-setup*))
  (%revert-working-copy (table chaperone-setup))) ; works

@export
(defun working-play-area-size (&key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-objects ((x :float)
                              (z :float))
    (%get-working-play-area-size (table chaperone-setup) x z)
    (values (cffi:mem-ref x :float) (cffi:mem-ref z :float)))) ; works

@export
(defun working-play-area-rect (&key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (pointer '(:struct hmd-quad-t))
    (%get-working-play-area-rect (table chaperone-setup) pointer)
    (cffi:mem-ref pointer '(:struct hmd-quad-t)))) ; works

@export
(defun working-collision-bounds-info (&key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (int-pointer :uint32)
    (let* ((quad-count
            (progn
              (%get-working-collision-bounds-info (table chaperone-setup)
                                                  (cffi:null-pointer)
                                                  int-pointer)
              (cffi:mem-ref int-pointer :uint32)))
           (result (make-array (list quad-count))))
      (cffi:with-foreign-object (pointer '(:struct hmd-quad-t) quad-count)
        (%get-working-collision-bounds-info (table chaperone-setup)
                                            pointer
                                            int-pointer)
        (loop for i from 0 below quad-count
              do (setf (aref result i)
                       (cffi:mem-aref pointer '(:struct hmd-quad-t) i))
              finally (return result)))))) ; works

@export
(defun live-collision-bounds-info (&key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (int-pointer :uint32)
    (let* ((quad-count
             (progn
               (%get-live-collision-bounds-info (table chaperone-setup)
                                                   (cffi:null-pointer)
                                                   int-pointer)
               (cffi:mem-ref int-pointer :uint32)))
           (result (make-array (list quad-count))))
      (cffi:with-foreign-object (pointer '(:struct hmd-quad-t) quad-count)
        (%get-live-collision-bounds-info (table chaperone-setup)
                                            pointer
                                            int-pointer)
        (loop for i from 0 below quad-count
              do (setf (aref result i)
                       (cffi:mem-aref pointer '(:struct hmd-quad-t) i))
              finally (return result)))))) ; works

@export
(defun working-seated-zero-pose-to-raw-tracking-pose (&key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (pointer '(:struct hmd-matrix-34-t))
    (%get-working-seated-zero-pose-to-raw-tracking-pose
     (table chaperone-setup) pointer)
    (cffi:mem-ref pointer '(:struct hmd-matrix-34-t)))) ; works

@export
(defun working-standing-zero-pose-to-raw-tracking-pose (&key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (pointer '(:struct hmd-matrix-34-t))
    (%get-working-standing-zero-pose-to-raw-tracking-pose
     (table chaperone-setup) pointer)
    (cffi:mem-ref pointer '(:struct hmd-matrix-34-t)))) ; works

@export
(defun set-working-play-area-size (x z &key (chaperone-setup *chaperone-setup*))
  (%set-working-play-area-size (table chaperone-setup) x z)) ; works

@export
(defun set-working-collision-bounds-info (quads &key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (pointer '(:struct hmd-quad-t) (length quads))
    (loop for i from 0 below (length quads)
          do (setf (cffi:mem-aref pointer '(:struct hmd-quad-t) i) (aref quads i)))
    (%set-working-collision-bounds-info (table chaperone-setup) pointer (length quads)))) ; not tested yet

@export
(defun set-working-perimeter (points &key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (pointer '(:struct hmd-vector-2-t) (length points))
    (loop for i from 0 below (length points)
          do (setf (cffi:mem-aref pointer '(:struct hmd-vector-2-t) i) (aref points i)))
    (%set-working-perimeter (table chaperone-setup) pointer (length points)))) ; not tested yet

@export
(defun set-working-seated-zero-pose-to-raw-tracking-pose
    (pose &key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (pointer '(:struct hmd-matrix-34-t))
    (setf (cffi:mem-ref pointer '(:struct hmd-matrix-34-t))
          pose)
    (%set-working-seated-zero-pose-to-raw-tracking-pose (table chaperone-setup) pose)))

@export
(defun set-working-standing-zero-pose-to-raw-tracking-pose
    (pose &key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (pointer '(:struct hmd-matrix-34-t))
    (setf (cffi:mem-ref pointer '(:struct hmd-matrix-34-t))
          pose)
    (%set-working-standing-zero-pose-to-raw-tracking-pose (table chaperone-setup) pose)))

@export
(defun reload-from-disk (configuration-file &key (chaperone-setup *chaperone-setup*))
  (%reload-from-disk (table chaperone-setup) configuration-file))

@export
(defun live-seated-zero-pose-to-raw-tracking-pose (&key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-object (pointer '(:struct hmd-matrix-34-t))
    (%get-live-seated-zero-pose-to-raw-tracking-pose (table chaperone-setup) pointer)
    (cffi:mem-ref pointer '(:struct hmd-matrix-34-t))))

@export
(defun export-live (&key (chaperone-setup *chaperone-setup*))
  "Returns a sexp that contains the current chaperone setup."
  (let ((length
          (cffi:with-foreign-object (pointer :uint32)
            (setf (cffi:mem-ref pointer :uint32) 0)
            (%export-live-to-buffer (table chaperone-setup) (cffi:null-pointer) pointer)
            (cffi:mem-ref pointer :uint32))))
    (cffi:with-foreign-string (foreign-string (make-string length))
      (cffi:with-foreign-object (pointer :uint32)
        (setf (cffi:mem-ref pointer :uint32) length)
        (if (%export-live-to-buffer (table chaperone-setup)
                                    foreign-string pointer)
            (cl-json:decode-json-from-source (cffi:foreign-string-to-lisp foreign-string))
            (error "%export-live-to-buffer returned NIL")))))) ; works

@export
(defun import-to-working (chaperone-configuration &key (chaperone-setup *chaperone-setup*))
  (cffi:with-foreign-string (pointer (cl-json:encode-json-to-string chaperone-configuration))
    (unless (%import-from-buffer-to-working (table chaperone-setup) pointer 1)
      (error "%import-from-buffer-to-working returned NIL")))) ; appears to work

@export
(defun show-working-set-preview (&key (chaperone-setup *chaperone-setup*))
  "Shows the chaperone data in the working set to preview in the compositor."
  (%show-working-set-preview (table chaperone-setup))) ; works

@export
(defun hide-working-set-preview (&key (chaperone-setup *chaperone-setup*))
  "Hides the chaperone data in the working set to preview in the compositor (if it was visible)."
  (%hide-working-set-preview (table chaperone-setup))) ; works

@export
(defun room-setup-starting (&key (chaperone-setup *chaperone-setup*))
  "Fire an event that the tracking system can use to know room setup is about to begin. This lets 
   the tracking system make any last minute adjustments that should be incorporated into the new
   setup.  If the user is adjusting live in HMD using a tweak tool, keep in mind that calling this
   might cause the user to see the room jump."
  (%room-setup-starting (table chaperone-setup))) ; works

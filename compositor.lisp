;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the IVRCompositor API.

;;;IVR_Compositor_022

(in-package 3b-openvr)

(defun submit (eye texture &key (compositor *compositor*)
                                (default-p t)
                                (lens-distortion-already-applied-p nil)
                                (gl-render-buffer-p nil)
                                (texture-with-pose-p nil)
                                (texture-with-depth-p nil)
                                (color-space :gamma)
                                (texture-type :open-gl)
                                (texture-bounds nil))
  (cffi:with-foreign-objects ((texture-pointer '(:struct texture-t))
                              (bounds-pointer '(:struct vr-texture-bounds-t))
                              (handle-pointer :int))
    (setf (cffi:mem-aref handle-pointer :int) texture)
    (let ((flags 0))
      (unless default-p 
        (when lens-distortion-already-applied-p (setf flags (logior flags 1)))
        (when gl-render-buffer-p (setf flags (logior flags 2)))
        (when texture-with-pose-p (setf flags (logior flags 8)))
        (when texture-with-depth-p (setf flags (logior flags 16))))
      (when texture-bounds
        (setf (cffi:mem-ref bounds-pointer '(:struct vr-texture-bounds-t)) texture-bounds))
      (let ((space color-space)) ; ugly.
        (cffi:with-foreign-slots ((handle type color-space) texture-pointer (:struct texture-t))
          (setf type texture-type
                handle (cffi:make-pointer texture)
                color-space space)))
     ; (break)
      (%submit (table compositor)
               eye
               texture-pointer
               (if texture-bounds bounds-pointer (cffi:null-pointer))
               flags))))

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
                   (when (cffi:foreign-slot-value p '(:struct tracked-device-pose-t) 'pose-is-valid)
                     (cffi:mem-ref p '(:struct tracked-device-pose-t)))))
    (loop for i below (length game-pose-array)
          for p = (cffi:mem-aptr pgpa '(:struct tracked-device-pose-t) i)
          do (setf (aref game-pose-array i) (cffi:mem-ref p '(:struct tracked-device-pose-t))))))

(defun get-last-pose (tracked-device &key (compositor *compositor*))
  (cffi:with-foreign-objects
      ((output-pose-pointer '(:struct tracked-device-pose-t))
       (output-game-pose-pointer '(:struct tracked-device-pose-t)))
    (%get-last-pose-for-tracked-device-index
     (table compositor)
     (tracked-device-index tracked-device)
     output-pose-pointer
     output-game-pose-pointer)
    (values (cffi:mem-ref output-pose-pointer '(:struct tracked-device-pose-t))
            (cffi:mem-ref output-game-pose-pointer '(:struct tracked-device-pose-t)))))

(define-clos-wrapper (compositor-frame-timing compositor-frame-timing) ()
                     ((size size)
                      (frame-index frame-index)
                      (number-of-times-presented num-frame-presents)
                      (number-of-times-mispresented num-mis-presented)
                      (number-of-times-dropped num-dropped-frames)
                      (reprojection-flags reprojection-flags)
                      (system-time system-time-in-seconds)
                      (pre-submit-gpu-duration pre-submit-gpu-ms)
                      (post-submit-gpu-duration post-submit-gpu-ms)
                      (total-render-gpu-duration total-render-gpu-ms)
                      (render-cpu-duration compositor-render-cpu-ms)
                      (render-gpu-duration compositor-render-gpu-ms)
                      (idle-duration compositor-idle-cpu-ms)
                      (client-frame-interval client-frame-interval-ms)
                      (present-call-cpu-duration present-call-cpu-ms)
                      (wait-for-present-duration wait-for-present-cpu-ms)
                      (submit-frame-duration submit-frame-ms)
                      (wait-get-poses-time wait-get-poses-called-ms)
                      (new-poses-ready-time new-poses-ready-ms)
                      (new-frame-ready-time new-frame-ready-ms)
                      (compositor-update-start-time compositor-update-start-ms)
                      (compositor-update-end-time compositor-update-end-ms)
                      (compositor-render-start-time compositor-render-start-ms)
                      (hmd-pose hmd-pose)
                      (number-of-vsyncs-ready-to-use num-vsyncs-ready-for-use)
                      (number-of-vsyncs-to-first-view num-vsyncs-to-first-view)))

(defun frame-timing (frames-ago &key (compositor *compositor*))
  (cffi:with-foreign-object (timing-pointer '(:struct compositor-frame-timing))
    (cffi:with-foreign-slots ((size) timing-pointer (:struct compositor-frame-timing))
      (setf size (cffi:foreign-type-size '(:struct compositor-frame-timing))))
    (%get-frame-timing (table compositor) timing-pointer frames-ago)
    (cffi:mem-ref timing-pointer '(:struct compositor-frame-timing))))

(defun frame-timings (number-of-frames &key (compositor *compositor*))
  (cffi:with-foreign-object (timing-pointer '(:struct compositor-frame-timing) number-of-frames)
    (cffi:with-foreign-slots ((size) timing-pointer (:struct compositor-frame-timing))
      (setf size (cffi:foreign-type-size 'compositor-frame-timing-tclass)))
    (%get-frame-timings (table compositor) timing-pointer number-of-frames)))

(define-clos-wrapper (cumulative-statistics compositor-cumulative-stats) ()
                     ((pid pid)
                      (number-of-frames-presented num-frame-presents)
                      (number-of-frames-dropped num-dropped-frames)
                      (number-of-frames-reprojected num-reprojected-frames)
                      (number-of-frames-presented-on-startup num-frame-presents-on-startup)
                      (number-of-frames-dropped-on-startup num-dropped-frames-on-startup)
                      (number-of-frames-reprojected-on-startup num-reprojected-frames-on-startup)
                      (number-of-frames-loading num-loading)
                      (number-of-frames-presented-loading num-frame-presents-loading)
                      (number-of-frames-dropped-loading num-dropped-frames-loading)
                      (number-of-frames-reprojected-loading num-reprojected-frames-loading)
                      (number-of-frames-timed-out num-timed-out)
                      (number-of-frames-presented-timed-out num-frame-presents-timed-out)
                      (number-of-frames-dropped-timed-out num-dropped-frames-timed-out)
                      (number-of-frames-reprojected-timed-out num-reprojected-frames-timed-out)))

(defun cumulative-stats (&key (compositor *compositor*))
  (cffi:with-foreign-object (stats-pointer '(:struct compositor-cumulative-stats))
    (%get-cumulative-stats (table compositor)
                           stats-pointer
                           (cffi:foreign-type-size '(:struct compositor-cumulative-stats)))
    (cffi:mem-ref stats-pointer '(:struct compositor-cumulative-stats))))


(defun fade-color (&key (background-p nil) (compositor *compositor*))
  (break)
  (let ((pointer (%get-current-fade-color (table compositor) background-p)))
    (break)
    (cffi:with-foreign-slots ((r g b a) pointer (:struct hmd-color-t))
      (values r g b a))))

(defun override-skybox (textures &key (color-space :gamma) (texture-type :open-gl) (compositor *compositor*))
  (cffi:with-foreign-object (texture-pointer '(:struct texture-t) (length textures))
    (let ((space color-space)) ;ugly
      (loop for i below (length textures)
            do (cffi:with-foreign-slots ((handle type color-space) texture-pointer (:struct texture-t))
                 (setf type texture-type
                       handle (cffi:make-pointer (aref textures i))
                       color-space space))))
    (%set-skybox-override (table compositor) texture-pointer (length textures))))

(defun set-tracking-space (origin &key (compositor *compositor*))
  (%set-tracking-space (table compositor) origin))

(defun tracking-space (&key (compositor *compositor*))
  (%get-tracking-space (table compositor)))

(defun clear-last-submitted-frame (&key (compositor *compositor*))
  (%clear-last-submitted-frame (table compositor)))

(defun post-present-handoff (&key (compositor *compositor*))
  (%post-present-handoff (table compositor)))

(defun frame-time-remaining (&key (compositor *compositor*))
  (%get-frame-time-remaining (table compositor)))

(defun fade-to-color (time red green blue alpha background-p &key (compositor *compositor*))
  (%fade-to-color (table compositor) time red green blue alpha background-p))

(defun fade-grid (seconds fade-in-p &key (compositor *compositor*))
  (%fade-grid (table compositor) seconds fade-in-p))

(defun grid-alpha (&key (compositor *compositor*))
  (%get-current-grid-alpha (table compositor)))

(defun clear-skybox-override (&key (compositor *compositor*))
  (%clear-skybox-override (table compositor)))

(defun bring-to-front (&key (compositor *compositor*))
  (%compositor-bring-to-front (table compositor)))

(defun go-to-back (&key (compositor *compositor*))
  (%compositor-go-to-back (table compositor)))

(defun quit-compositor (&key (compositor *compositor*))
  (%compositor-quit (table compositor)))

(defun fullscreen-p (&key (compositor *compositor*))
  (%is-fullscreen (table compositor)))

(defun current-scene-focus-process (&key (compositor *compositor*))
  (%get-current-scene-focus-process (table compositor)))

(defun last-frame-renderer (&key (compositor *compositor*))
  (%get-last-frame-renderer (table compositor)))

(defun can-render-scene-p (&key (compositor *compositor*))
  (%can-render-scene (table compositor)))

(defun show-mirror-window (&key (compositor *compositor*))
  (%show-mirror-window (table compositor)))
(defun hide-mirror-window (&key (compositor *compositor*))
  (%hide-mirror-window (table compositor)))
(defun mirror-window-visible-p (&key (compositor *compositor*))
  (%is-mirror-window-visible (table compositor)))

(defun dump-images (&key (compositor *compositor*))
  (%compositor-dump-images (table compositor)))

(defun should-app-render-with-low-resources-p (&key (compositor *compositor*))
  (%should-app-render-with-low-resources (table compositor)))

(defun force-interleaved-reprojection (override-p &key (compositor *compositor*))
  (%force-interleaved-reprojection-on (table compositor) override-p))

(defun force-reconnect-process (&key (compositor *compositor*))
  (%force-reconnect-process (table compositor)))

(defun suspend-rendering (&key (suspend-p t) (compositor *compositor*))
  (%suspend-rendering (table compositor) suspend-p))

(defun mirror-gl-texture (&key (compositor *compositor*))
  (error "implement me"))

(defun release-shared-gl-texture (&key (compositor *compositor*))
  (error "implement me"))

(defun lock-gl-shared-texture (&key (compositor *compositor*))
  (error "implement me"))

(defun unlock-gl-shared-texture (&key (compositor *compositor*))
  (error "implement me"))

(defun set-explicit-timing-mode (timing-mode &key (compositor *compositor*))
  (%set-explicit-timing-mode (table compositor) timing-mode))

(defun submit-explicit-timing-data (&key (compositor *compositor*))
  (%submit-explicit-timing-data (table compositor)))

(defun motion-smoothing-enabled-p (&key (compositor *compositor*))
  (%is-motion-smoothing-enabled (table compositor)))

(defun motion-smoothing-supported-p (&key (compositor *compositor*))
  (%is-motion-smoothing-supported (table compositor)))

(defun current-scene-focus-app-loading-p (&key (compositor *compositor*))
  (%is-current-scene-focus-app-loading (table compositor)))

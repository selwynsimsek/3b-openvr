;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the Overlay API.

;;; IVR_Overlay_022

(in-package :3b-openvr)

(defmacro with-overlay-error (&rest body)
  (let ((error-name (gensym "error-value")))
    `(let ((,error-name (progn ,@body)))
       (if (eq ,error-name :none) t (error "VR overlay error: ~a" ,error-name)))))

;; overlay management methods

(defun find-overlay (overlay-key &key (overlay *overlay*))
  "Finds an existing overlay with the specified key."
  (cffi:with-foreign-object (handle 'vr-overlay-handle-t)
    (with-overlay-error
        (%find-overlay (table overlay) overlay-key handle)
      (cffi:mem-ref handle 'vr-overlay-handle-t))))

(defun create-overlay (overlay-key overlay-friendly-name &key (overlay *overlay*))
  "Creates a new named overlay. All overlays start hidden and with default settings."
  (cffi:with-foreign-object (handle 'vr-overlay-handle-t)
    (with-overlay-error
        (%create-overlay (table overlay) overlay-key overlay-friendly-name handle)
      (cffi:mem-ref handle 'vr-overlay-handle-t))))

(defun destroy-overlay (overlay-handle &key (overlay *overlay*))
  "Destroys the specified overlay. When an application calls VR_Shutdown all overlays created by that app
   are automatically destroyed."
  (with-overlay-error (%destroy-overlay (table overlay) overlay-handle)))

(defun overlay-key (overlay-handle &key (overlay *overlay*))
  "Returns the string key of the overlay."
  (cffi:with-foreign-string (pointer (make-string +vr-overlay-max-key-length+))
    (cffi:with-foreign-object (error-pointer 'vr-overlay-error)
      (%get-overlay-key (table overlay) overlay-handle pointer +vr-overlay-max-key-length+ error-pointer)
      (unless (eq :none (cffi:mem-ref error-pointer 'vr-overlay-error))
        (error "VR overlay error: ~a" (cffi:mem-ref error-pointer 'vr-overlay-error)))
      (cffi:foreign-string-to-lisp pointer))))

(defun overlay-name (overlay-handle &key (overlay *overlay*))
  "Returns the friendly name of the overlay."
  (cffi:with-foreign-string (pointer (make-string +vr-overlay-max-name-length+))
    (cffi:with-foreign-object (error-pointer 'vr-overlay-error)
      (%get-overlay-name (table overlay) overlay-handle pointer +vr-overlay-max-name-length+ error-pointer)
      (unless (eq :none (cffi:mem-ref error-pointer 'vr-overlay-error))
        (error "VR overlay error: ~a" (cffi:mem-ref error-pointer 'vr-overlay-error)))
      (cffi:foreign-string-to-lisp pointer))))

(defun set-overlay-name (overlay-handle name &key (overlay *overlay*))
  "Set the name to use for this overlay."
  (with-overlay-error (%set-overlay-name (table overlay) overlay-handle name)))

(defun overlay-image-data (overlay-handle &key (overlay *overlay*))
  "Gets the raw image data from an overlay. Overlay image data is always returned as RGBA data, 4 bytes per pixel."
  (cffi:with-foreign-objects ((width-pointer :uint32)
                              (height-pointer :uint32))
    (setf (cffi:mem-ref width-pointer :uint32) 0
          (cffi:mem-ref height-pointer :uint32) 0)
    (%get-overlay-image-data
     (table overlay) overlay-handle (cffi:null-pointer) 0 width-pointer height-pointer)
    (let* ((width (cffi:mem-ref width-pointer :uint32))
           (height (cffi:mem-ref height-pointer :uint32))
           (buffer (cffi:make-shareable-byte-vector (* 4 width height))))
      (cffi:with-pointer-to-vector-data (buffer-pointer buffer)
        (with-overlay-error
            (%get-overlay-image-data (table overlay) overlay-handle buffer-pointer
                                     (* 4 width height) width-pointer height-pointer))
        (values buffer width height)))))

;; overlay rendering methods

(defun set-overlay-rendering-pid (overlay-handle pid &key (overlay *overlay*)))
(defun overlay-rendering-pid (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-flag (overlay-handle flag enabled-p &key (overlay *overlay*)))
(defun overlay-flag (overlay-handle flag &key (overlay *overlay*)))
(defun overlay-flags (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-color (overlay-handle red green blue &key (overlay *overlay*)))
(defun overlay-color (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-alpha (overlay-handle alpha &key (overlay *overlay*)))
(defun overlay-alpha (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-texel-aspect (overlay-handle texel-aspect &key (overlay *overlay*)))
(defun overlay-texel-aspect (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-sort-order (overlay-handle sort-order &key (overlay *overlay*)))
(defun overlay-sort-order (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-width-in-meters (overlay-handle width-in-meters &key (overlay *overlay*)))
(defun overlay-width-in-meters (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-curvature (overlay-handle curvature &key (overlay *overlay*)))
(defun overlay-curvature (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-texture-color-space (overlay-handle color-space &key (overlay *overlay*)))
(defun overlay-texture-color-space (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-texture-bounds (overlay-handle texture-bounds &key (overlay *overlay*)))
(defun overlay-texture-bounds (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-render-model (overlay-handle render-model-name color &key (overlay *overlay*)))
(defun overlay-render-model (overlay-handle &key (overlay *overlay*)))
(defun overlay-transform-type (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-transform-absolute (overlay-handle origin tracking-origin-to-overlay-transform
                                       &key (overlay *overlay*)))
(defun overlay-transform-absolute (overlay-handle origin &key (overlay *overlay*)))
(defun set-overlay-transform-tracked-device-relative (overlay-handle tracked-device
                                                      tracked-device-to-overlay-transform
                                                      &key (overlay *overlay*)))
(defun overlay-transform-tracked-device-relative (overlay-handle tracked-device
                                                  &key (overlay *overlay*)))
(defun set-overlay-transform-tracked-device-component (overlay-handle device-index component-name
                                                       &key (overlay *overlay*)))
(defun overlay-transform-tracked-device-component (overlay-handle &key (overlay *overlay*)))
(defun overlay-transform-overlay-relative (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-transform-overlay-relative (overlay-handle parent-overlay-handle
                                               parent-overlay-to-overlay-transform
                                               &key (overlay *overlay*)))
(defun set-overlay-transform-cursor (cursor-overlay-handle hotspot &key (overlay *overlay*)))
(defun overlay-transform-cursor (overlay-handle &key (overlay *overlay*)))
(defun show-overlay (overlay-handle &key (overlay *overlay*)))
(defun hide-overlay (overlay-handle &key (overlay *overlay*)))
(defun overlay-visible-p (overlay-handle &key (overlay *overlay*)))
(defun transfrom-for-overlay-coordinates (overlay-handle tracking-origin coordinates-in-overlay
                                          &key (overlay *overlay*)))

;; overlay input methods
(defun poll-next-overlay-event (overlay-handle &key (overlay *overlay*)))
(defun overlay-input-method (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-input-method (overlay-handle input-method &key (overlay *overlay*)))
(defun overlay-mouse-scale (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-mouse-scale (overlay-handle mouse-scale &key (overlay *overlay*)))
(defun compute-overlay-intersection (overlay-handle parameters &key (overlay *overlay*)))
(defun hover-target-overlay-p (overlay-handle &key (overlay *overlay*)))
(defun set-overlay-dual-analog-transform (overlay-handle which center radius
                                          &key (overlay *overlay*)))
(defun overlay-dual-analog-transform (overlay-handle which &key (overlay *overlay*)))
(defun set-overlay-intersection-mask (overlay-handle mask-primitives &key (overlay *overlay*)))
(defun trigger-laser-mouse-haptic-vibration (overlay-handle duration-in-seconds frequency amplitude
                                             &key (overlay *overlay*)))
(defun set-overlay-cursor (overlay-handle cursor-handle &key (overlay *overlay*)))
(defun set-overlay-cursor-position-override (overlay-handle cursor &key (overlay *overlay*)))
(defun clear-overlay-cursor-position-override (&key (overlay *overlay*)))

;; overlay texture methods

(defun set-overlay-texture (overlay-handle texture &key (overlay *overlay*)
                                                        (texture-type :open-gl)
                                                        (color-space :gamma))
  "Texture to draw for the overlay. This function can only be called by the overlay's creator or renderer process
   (see #'set-overlay-rendering-pid)"
  (cffi:with-foreign-objects ((texture-pointer '(:struct texture-t)))
    (let ((space color-space))
      (cffi:with-foreign-slots ((handle type color-space) texture-pointer '(:struct texture-t))
        (setf type texture-type
              handle (cffi:make-pointer texture)
              color-space space)
        (with-overlay-error
            (%set-overlay-texture (table overlay) overlay-handle texture-pointer))))))

(defun clear-overlay-texture (overlay-handle &key (overlay *overlay*))
  "Use this to tell the overlay system to release the texture set for this overlay."
  (with-overlay-error
      (%clear-overlay-texture (table overlay) overlay-handle)))

(defun set-overlay-raw (overlay-handle buffer width height bytes-per-pixel
                        &key (overlay *overlay*))
  "Separate interface for providing the data as a stream of bytes, but there is an upper bound on data 
  that can be sent. This function can only be called by the overlay's renderer process. buffer must be a shareable
  byte vector."
  (cffi:with-pointer-to-vector-data (pointer buffer)
    (with-overlay-error
      (%set-overlay-raw (table overlay) overlay-handle pointer width height bytes-per-pixel))))

(defun set-overlay-from-file (overlay-handle file-path &key (overlay *overlay*))
  "Separate interface for providing the image through a filename: can be png or jpg,
   and should not be bigger than 1920x1080.This function can only be called by the overlay's renderer process."
  (with-overlay-error
      (%set-overlay-from-file (table overlay) overlay-handle file-path)))

;;(defun overlay-texture (overlay-handle)) ;??
;;(defun release-native-overlay-handle) ;??
(defun overlay-texture-size (overlay-handle &key (overlay *overlay*))
  (cffi:with-foreign-objects ((width :uint32)
                              (height :uint32))
    (with-overlay-error
        (%get-overlay-texture-size (table overlay) overlay-handle width height))
    (values (cffi:mem-ref width :uint32) (cffi:mem-ref height :uint32))))

;; dashboard overlay methods

(defun create-dashboard-overlay (overlay-key overlay-friendly-name &key (overlay *overlay*))
  (cffi:with-foreign-objects ((main-pointer 'vr-overlay-handle-t)
                              (thumbnail-pointer 'vr-overlay-handle-t))
    (with-overlay-error
        (%create-dashboard-overlay (table overlay) overlay-key overlay-friendly-name
                                   main-pointer thumbnail-pointer))
    (values (cffi:mem-ref main-pointer 'vr-overlay-handle-t)
            (cffi:mem-ref thumbnail-pointer 'vr-overlay-handle-t))))

(defun dashboard-visible-p (&key (overlay *overlay*))
  "Returns true if the dashboard is visible."
  (%is-dashboard-visible (table overlay)))

(defun dashboard-overlay-active-p (overlay-handle &key (overlay *overlay*))
  (%is-active-dashboard-overlay (table overlay) overlay-handle))

(defun set-dashboard-overlay-scene-process (overlay-handle process-id &key (overlay *overlay*))
  "Sets the dashboard overlay to only appear when the specified process ID has scene focus."
  (with-overlay-error
      (%set-dashboard-overlay-scene-process (table overlay) overlay-handle process-id)))

(defun dashboard-overlay-scene-process (overlay-handle &key (overlay *overlay*))
  (cffi:with-foreign-object (pointer :uint32)
    (with-overlay-error
        (%get-dashboard-overlay-scene-process (table overlay) overlay-handle pointer))
    (cffi:mem-ref pointer :uint32)))

(defun show-dashboard (overlay-to-show &key (overlay *overlay*))
  (%show-dashboard (table overlay) overlay-to-show))

(defun primary-dashboard-device (&key (overlay *overlay*))
  "Returns the tracked device that has the laser pointer in the dashboard."
  (%get-primary-dashboard-device (table overlay)))

;; keyboard methods

(defun show-keyboard (input-mode line-input-mode description max-char existing-text
                      use-minimal-mode-p user-value &key (overlay *overlay*))
  "Show the virtual keyboard to accept input."
  (with-overlay-error
      (%show-keyboard (table origin) input-mode line-input-mode description max-char existing-text
                      use-minimal-mode-p user-value)))

(defun show-keyboard-for-overlay (overlay-handle input-mode line-input-mode description max-char
                                  existing-text use-minimal-mode-p user-value
                                  &key (overlay *overlay*))
  (with-overlay-error
      (%show-keyboard-for-overlay (table origin) overlay-handle input-mode line-input-mode description
                                  max-char existing-text use-minimal-mode-p user-value)))

(defun keyboard-text (&key (overlay *overlay*) (buffer-size 512))
  "Get the text that was entered into the text input."
  (cffi:with-foreign-string (string-pointer (make-string buffer-size))
    (%get-keyboard-text (table overlay) string-pointer buffer-size)
    (cffi:foreign-string-to-lisp string-pointer)))

(defun hide-keyboard (&key (overlay *overlay*))
  "Hide the virtual keyboard."
  (%hide-keyboard (table overlay)))

(defun set-keyboard-transform-absolute (origin tracking-origin-to-keyboard-transform
                                        &key (overlay *overlay*))
  "Set the position of the keyboard in world space."
  (%set-keyboard-transform-absolute (table overlay) origin tracking-origin-to-keyboard-transform))  ; need a type translator?

(defun set-keyboard-position-for-overlay (overlay-handle avoid-rect &key (overlay *overlay*))
  "Set the position of the keyboard in overlay space by telling it to avoid a rectangle in the overlay.
 Rectangle coords have (0,0) in the bottom left."
  (%set-keyboard-position-for-overlay (table overlay) overlay-handle avoid-rect)) ; need a type translator?

;; message box methods

(defun show-message-overlay (text caption button-0-text button-1-text button-2-text button-3-text
                             &key (overlay *overlay*))
  "Show the message overlay. This will block and return you a result."
  (%show-message-overlay (table overlay) text caption button-0-text button-1-text button-2-text button-3-text))

(defun close-message-overlay (&key (overlay *overlay*))
  "If the calling process owns the overlay and it's open, this will close it."
  (%close-message-overlay (table overlay)))

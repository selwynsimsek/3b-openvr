;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the Overlay API.

;;; IVR_Overlay_022

(in-package :3b-openvr)

(annot:enable-annot-syntax)

(defmacro with-overlay-error (&rest body)
  (let ((error-name (gensym "error-value")))
    `(let ((,error-name (progn ,@body)))
       (if (eq ,error-name :none) t (error "VR overlay error: ~a" ,error-name)))))

;; overlay management methods

@export
(defun find-overlay (overlay-key &key (overlay *overlay*))
  "Finds an existing overlay with the specified key."
  (cffi:with-foreign-object (handle 'vr-overlay-handle-t)
    (with-overlay-error
        (%find-overlay (table overlay) overlay-key handle)
      (cffi:mem-ref handle 'vr-overlay-handle-t))))

@export
(defun create-overlay (overlay-key overlay-friendly-name &key (overlay *overlay*))
  "Creates a new named overlay. All overlays start hidden and with default settings."
  (cffi:with-foreign-object (handle 'vr-overlay-handle-t)
    (with-overlay-error
        (%create-overlay (table overlay) overlay-key overlay-friendly-name handle)
      (cffi:mem-ref handle 'vr-overlay-handle-t))))

@export
(defun destroy-overlay (overlay-handle &key (overlay *overlay*))
  "Destroys the specified overlay. When an application calls VR_Shutdown all overlays created by that app
   are automatically destroyed."
  (with-overlay-error (%destroy-overlay (table overlay) overlay-handle)))

@export
(defun overlay-key (overlay-handle &key (overlay *overlay*))
  "Returns the string key of the overlay."
  (cffi:with-foreign-string (pointer (make-string +vr-overlay-max-key-length+))
    (cffi:with-foreign-object (error-pointer 'vr-overlay-error)
      (%get-overlay-key (table overlay) overlay-handle pointer +vr-overlay-max-key-length+ error-pointer)
      (unless (eq :none (cffi:mem-ref error-pointer 'vr-overlay-error))
        (error "VR overlay error: ~a" (cffi:mem-ref error-pointer 'vr-overlay-error)))
      (cffi:foreign-string-to-lisp pointer))))

@export
(defun overlay-name (overlay-handle &key (overlay *overlay*))
  "Returns the friendly name of the overlay."
  (cffi:with-foreign-string (pointer (make-string +vr-overlay-max-name-length+))
    (cffi:with-foreign-object (error-pointer 'vr-overlay-error)
      (%get-overlay-name (table overlay) overlay-handle pointer +vr-overlay-max-name-length+ error-pointer)
      (unless (eq :none (cffi:mem-ref error-pointer 'vr-overlay-error))
        (error "VR overlay error: ~a" (cffi:mem-ref error-pointer 'vr-overlay-error)))
      (cffi:foreign-string-to-lisp pointer))))

@export
(defun set-overlay-name (overlay-handle name &key (overlay *overlay*))
  "Set the name to use for this overlay."
  (with-overlay-error (%set-overlay-name (table overlay) overlay-handle name)))

@export
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

@export
(defun set-overlay-rendering-pid (overlay-handle pid &key (overlay *overlay*))
  "Sets the pid that is allowed to render to this overlay (the creator pid is always allow to render),
   by default this is the pid of the process that made the overlay."
  (with-overlay-error
      (%set-overlay-rendering-pid (table overlay) overlay-handle pid)))

@export
(defun overlay-rendering-pid (overlay-handle &key (overlay *overlay*))
  "Gets the pid that is allowed to render to this overlay."
  (%get-overlay-rendering-pid (table overlay) overlay-handle))

@export
(defun set-overlay-flag (overlay-handle flag enabled-p &key (overlay *overlay*))
  "Specify flag setting for a given overlay."
  (with-overlay-error
      (%set-overlay-flag (table overlay) overlay-handle flag enabled-p)))

@export
(defun overlay-flag (overlay-handle flag &key (overlay *overlay*))
  "Gets flag setting for a given overlay."
  (cffi:with-foreign-object (pointer :bool)
    (with-overlay-error
        (%get-overlay-flag (table overlay) overlay-handle flag pointer))
    (cffi:mem-ref pointer :bool)))

@export
(defun overlay-flags (overlay-handle &key (overlay *overlay*))
  "Gets all the flags for a given overlay."
  (cffi:with-foreign-object (pointer :uint32)
    (with-overlay-error
        (%get-overlay-flags (table overlay) overlay-handle pointer))
    (cffi:mem-ref pointer :uint32)))

@export
(defun set-overlay-color (overlay-handle red green blue &key (overlay *overlay*))
  "Sets the color tint of the overlay quad. Use 0.0 to 1.0 per channel."
  (with-overlay-error (%set-overlay-color (table overlay) overlay-handle red green blue)))

@export
(defun overlay-color (overlay-handle &key (overlay *overlay*))
  "Gets the color tint of the overlay quad."
  (cffi:with-foreign-objects ((red :float)
                              (green :float)
                              (blue :float))
    (with-overlay-error (%get-overlay-color (table overlay) overlay-handle red green blue))
    (values (cffi:mem-ref red :float) (cffi:mem-ref green :float) (cffi:mem-ref blue :float))))

@export
(defun set-overlay-alpha (overlay-handle alpha &key (overlay *overlay*))
  "Sets the alpha of the overlay quad. Use 1.0 for 100 percent opacity to 0.0 for 0 percent opacity."
  (with-overlay-error (%set-overlay-alpha (table overlay) overlay-handle alpha)))

@export
(defun overlay-alpha (overlay-handle &key (overlay *overlay*))
  "Gets the alpha of the overlay quad. By default overlays are rendering at 100 percent alpha (1.0)."
  (cffi:with-foreign-object (alpha :float)
    (with-overlay-error (%get-overlay-alpha (table overlay) overlay-handle alpha))
    (cffi:mem-ref alpha :float)))

@export
(defun set-overlay-texel-aspect (overlay-handle texel-aspect &key (overlay *overlay*))
  "Sets the aspect ratio of the texels in the overlay. 1.0 means the texels are square. 2.0 means the texels
   are twice as wide as they are tall. Defaults to 1.0."
  (with-overlay-error (%set-overlay-texel-aspect (table overlay) overlay-handle texel-aspect)))

@export
(defun overlay-texel-aspect (overlay-handle &key (overlay *overlay*))
  "Gets the aspect ratio of the texels in the overlay. Defaults to 1.0."
  (cffi:with-foreign-object (texel-aspect :float)
    (with-overlay-error (%get-overlay-texel-aspect (table overlay) overlay-handle texel-aspect))
    (cffi:mem-ref texel-aspect :float)))


@export
(defun set-overlay-sort-order (overlay-handle sort-order &key (overlay *overlay*))
  "Sets the rendering sort order for the overlay. Overlays are rendered this order:
   Overlays owned by the scene application
   Overlays owned by some other application
   Within a category overlays are rendered lowest sort order to highest sort order. Overlays with the same 
   sort order are rendered back to front base on distance from the HMD.
   Sort order defaults to 0."
  (with-overlay-error (%set-overlay-sort-order (table overlay) overlay-handle sort-order)))

@export
(defun overlay-sort-order (overlay-handle &key (overlay *overlay*))
  "Gets the sort order of the overlay. See #'set-overlay-sort-order for how this works."
  (cffi:with-foreign-object (sort-order :uint32)
    (with-overlay-error
        (%get-overlay-sort-order (table overlay) overlay-handle sort-order))
    (cffi:mem-ref sort-order :uint32)))

@export
(defun set-overlay-width-in-meters (overlay-handle width-in-meters &key (overlay *overlay*))
  "Sets the width of the overlay quad in meters. By default overlays are rendered on a quad that is 1 meter across."
  (with-overlay-error (%set-overlay-width-in-meters (table overlay) overlay-handle width-in-meters)))

@export
(defun overlay-width-in-meters (overlay-handle &key (overlay *overlay*))
  "Returns the width of the overlay quad in meters. By default overlays are rendered on a quad that is 1 meter across."
  (cffi:with-foreign-object (width :float)
    (with-overlay-error
        (%get-overlay-width-in-meters (table overlay) overlay-handle width))
    (cffi:mem-ref width :float)))

@export
(defun set-overlay-curvature (overlay-handle curvature &key (overlay *overlay*))
  "Use to draw overlay as a curved surface. Curvature is a percentage from (0..1] where 1 is a fully closed cylinder.
   For a specific radius, curvature can be computed as: overlay.width / (2 PI r)."
  (with-overlay-error
    (%set-overlay-curvature (table overlay) overlay-handle curvature)))

@export
(defun overlay-curvature (overlay-handle &key (overlay *overlay*))
  "Returns the curvature of the overlay as a percentage from (0..1] where 1 is a fully closed cylinder."
  (cffi:with-foreign-object (curvature :float)
    (with-overlay-error
        (%get-overlay-curvature (table overlay) overlay-handle curvature))
    (cffi:mem-ref curvature :float)))

@export
(defun set-overlay-texture-color-space (overlay-handle color-space &key (overlay *overlay*))
  "Sets the colorspace the overlay texture's data is in.  Defaults to 'auto'.
   If the texture needs to be resolved, you should call #'set-overlay-texture with the appropriate colorspace instead."
  (with-overlay-error (%set-overlay-texture-color-space (table overlay) overlay-handle color-space)))

@export
(defun overlay-texture-color-space (overlay-handle &key (overlay *overlay*))
  "Gets the overlay's current colorspace setting."
  (cffi:with-foreign-object (color-space 'color-space)
    (with-overlay-error
        (%get-overlay-texture-color-space (table overlay) overlay-handle color-space))
    (cffi:mem-ref color-space 'color-space)))

@export
(defun set-overlay-texture-bounds (overlay-handle u-min v-min u-max v-max &key (overlay *overlay*))
  "Sets the part of the texture to use for the overlay. UV Min is the upper left corner and UV Max is the lower
   right corner."
  (cffi:with-foreign-object (texture-bounds '(:struct vr-texture-bounds-t))
    (setf (cffi:foreign-slot-value texture-bounds '(:struct vr-texture-bounds-t) 'u-min) u-min
          (cffi:foreign-slot-value texture-bounds '(:struct vr-texture-bounds-t) 'v-min) v-min
          (cffi:foreign-slot-value texture-bounds '(:struct vr-texture-bounds-t) 'u-max) u-max
          (cffi:foreign-slot-value texture-bounds '(:struct vr-texture-bounds-t) 'v-max) v-max)
    (with-overlay-error
      (%set-overlay-texture-bounds (table overlay) overlay-handle texture-bounds))))

@export
(defun overlay-texture-bounds (overlay-handle &key (overlay *overlay*))
  "Gets the part of the texture to use for the overlay. UV Min is the upper left corner and UV Max is the lower
   right corner."
  (cffi:with-foreign-object (texture-bounds '(:struct vr-texture-bounds-t))
    (with-overlay-error
        (%get-overlay-texture-bounds (table overlay) overlay-handle texture-bounds))
    (values (cffi:foreign-slot-value texture-bounds '(:struct vr-texture-bounds-t) 'u-min)
            (cffi:foreign-slot-value texture-bounds '(:struct vr-texture-bounds-t) 'v-min)
            (cffi:foreign-slot-value texture-bounds '(:struct vr-texture-bounds-t) 'u-max)
            (cffi:foreign-slot-value texture-bounds '(:struct vr-texture-bounds-t) 'v-max))))

(defun set-overlay-render-model (overlay-handle render-model-name color &key (overlay *overlay*))
  "Sets render model to draw behind this overlay and the vertex color to use, pass NIL for color to match the
   overlays vertex color. 
   The model is scaled by the same amount as the overlay, with a default of 1m."
  (cffi:with-foreign-object (foreign-color '(:struct hmd-color-t))
    (when color (setf (cffi:mem-ref foreign-color '(:struct hmd-color-t)) color))
    (with-overlay-error
        (%set-overlay-render-model (table overlay) overlay-handle render-model-name (if color foreign-color
                                                                                        (cffi:null-pointer))))))

(defun overlay-render-model (overlay-handle &key (overlay *overlay*))
  "Gets render model to draw behind this overlay"
  (cffi:with-foreign-objects ((error-pointer 'vr-overlay-error)
                              (color-pointer '(:struct hmd-color-t)))
    (setf (cffi:mem-ref error-pointer 'vr-overlay-error) :none)
    (let ((length (%get-overlay-render-model (table overlay) overlay-handle
                                             (cffi:null-pointer) 0 color-pointer error-pointer)))
      (unless (eq (cffi:mem-ref error-pointer 'vr-overlay-error) :none)
        (error "Overlay error: ~a" (cffi:mem-ref error-pointer 'vr-overlay-error)))
      (cffi:with-foreign-string (string-pointer (make-string length))
        (%get-overlay-render-model (table overlay) overlay-handle string-pointer
                                   length color-pointer error-pointer)
        (unless (eq (cffi:mem-ref error-pointer 'vr-overlay-error) :none)
          (error "Overlay error: ~a" (cffi:mem-ref error-pointer 'vr-overlay-error)))
        (values (cffi:foreign-string-to-lisp string-pointer)
                (cffi:mem-ref color-pointer '(:struct hmd-color-t)))))))

(defun overlay-transform-type (overlay-handle &key (overlay *overlay*))
  "Returns the transform type of this overlay."
  (cffi:with-foreign-object (transform-type 'vr-overlay-transform-type)
    (with-overlay-error
        (%get-overlay-transform-type (table overlay) overlay-handle transform-type))
    (cffi:mem-ref transform-type 'vr-overlay-transform-type)))

(defun set-overlay-transform-absolute (overlay-handle origin tracking-origin-to-overlay-transform
                                       &key (overlay *overlay*))
  "Sets the transform to absolute tracking origin."
  (with-overlay-error
      (%set-overlay-transform-absolute (table overlay) overlay-handle origin tracking-origin-to-overlay-transform)))

(defun overlay-transform-absolute (overlay-handle origin &key (overlay *overlay*))
  "Gets the transform if it is absolute. Returns an error if the transform is some other type."
  (cffi:with-foreign-objects ((tracking-origin 'tracking-universe-origin)
                              (transform '(:struct hmd-matrix-34-t)))
    (with-overlay-error
        (%get-overlay-transform-absolute (table overlay) overlay-handle tracking-origin transform))
    (values (cffi:mem-ref tracking-origin 'tracking-universe-origin)
            (cffi:mem-ref transform '(:struct hmd-matrix-34-t)))))

(defun set-overlay-transform-tracked-device-relative (overlay-handle tracked-device
                                                      tracked-device-to-overlay-transform
                                                      &key (overlay *overlay*))
  "Sets the transform to relative to the transform of the specified tracked device."
  (with-overlay-error
      (%set-overlay-transform-tracked-device-relative
       (table overlay) overlay-handle tracked-device tracked-device-to-overlay-transform)))

(defun overlay-transform-tracked-device-relative (overlay-handle
                                                  &key (overlay *overlay*))

  "Gets the transform if it is relative to another overlay. Returns an error if the transform is some other type."
  (cffi:with-foreign-objects ((transform '(:struct hmd-matrix-34-t))
                              (handle 'tracked-device-index-t))
    (with-overlay-error
        (%get-overlay-transform-tracked-device-relative
         (table overlay) overlay-handle handle transform))
    (values (cffi:mem-ref handle 'tracked-device-index-t)
            (cffi:mem-ref transform '(:struct hmd-matrix-34-t)))))

(defun set-overlay-transform-tracked-device-component (overlay-handle device-index component-name
                                                       &key (overlay *overlay*))
  "Sets the transform to draw the overlay on a rendermodel component mesh instead of a quad. This will only draw when
   the system is drawing the device. Overlays with this transform type cannot receive mouse events."
  (with-overlay-error
    (%set-overlay-transform-tracked-device-component (table overlay) overlay-handle device-index component-name)))

(defun overlay-transform-tracked-device-component (overlay-handle &key (overlay *overlay*) (buffer-size 512))
  "Gets the transform information when the overlay is rendering on a component."
  (cffi:with-foreign-object (device 'tracked-device-index-t)
    (cffi:with-foreign-string (foreign-string (make-string buffer-size))
      (with-overlay-error (%get-overlay-transform-tracked-device-component
                           (table overlay) overlay-handle device foreign-string buffer-size))
      (values (cffi:mem-ref device 'tracked-device-index-t)
              (cffi:foreign-string-to-lisp foreign-string)))))

(defun overlay-transform-overlay-relative (overlay-handle &key (overlay *overlay*))
  "Gets the transform if it is relative to another overlay. Returns an error if the transform is some other type."
  (cffi:with-foreign-objects ((foreign-matrix '(:struct hmd-matrix-34-t))
                              (foreign-handle 'vr-overlay-handle-t))
    (with-overlay-error
        (%get-overlay-transform-overlay-relative (table overlay) overlay-handle foreign-handle foreign-matrix))
    (values (cffi:mem-ref foreign-handle 'vr-overlay-handle-t)
            (cffi:mem-ref foreign-matrix '(:struct hmd-matrix-34-t)))))

(defun set-overlay-transform-overlay-relative (overlay-handle parent-overlay-handle
                                               parent-overlay-to-overlay-transform
                                               &key (overlay *overlay*))
  "Sets the transform to relative to the transform of the specified overlay. This overlays visibility will also
   track the parents visibility."
  (cffi:with-foreign-object (foreign-matrix '(:struct hmd-matrix-34-t))
    (setf (cffi:mem-ref foreign-matrix '(:struct hmd-matrix-34-t)) parent-overlay-to-overlay-transform)
    (with-overlay-error
      (%set-overlay-transform-overlay-relative (table overlay) overlay-handle parent-overlay-handle foreign-matrix))))

(defun set-overlay-transform-cursor (cursor-overlay-handle hotspot &key (overlay *overlay*))
  "Sets the hotspot for the specified overlay when that overlay is used as a cursor. These are in texture space
   with 0,0 in the upper left corner of the texture and 1,1 in the lower right corner of the texture."
  (cffi:with-foreign-object (foreign-hotspot '(:struct hmd-vector-2-t))
    (setf (cffi:mem-ref foreign-hotspot '(:struct hmd-vector-2-t)) hotspot)
    (with-overlay-error
      (%set-overlay-transform-cursor (table overlay) cursor-overlay-handle foreign-hotspot))))

(defun overlay-transform-cursor (overlay-handle &key (overlay *overlay*))
  "Gets cursor hotspot/transform for the specified overlay."
  (cffi:with-foreign-object (hotspot '(:struct hmd-vector-2-t))
    (with-overlay-error
        (%get-overlay-transform-cursor (table overlay) overlay-handle hotspot))
    (cffi:mem-ref hotspot '(:struct hmd-vector-2-t))))

@export
(defun show-overlay (overlay-handle &key (overlay *overlay*))
  "Shows the VR overlay.  For dashboard overlays, only the Dashboard Manager is allowed to call this."
  (with-overlay-error (%show-overlay (table overlay) overlay-handle)))

@export
(defun hide-overlay (overlay-handle &key (overlay *overlay*))
  
  "Hides the VR overlay.  For dashboard overlays, only the Dashboard Manager is allowed to call this."
  (with-overlay-error (%hide-overlay (table overlay) overlay-handle)))

@export
(defun overlay-visible-p (overlay-handle &key (overlay *overlay*))
  "Returns true if the overlay is visible."
  (%is-overlay-visible (table overlay) overlay-handle))

(defun transform-for-overlay-coordinates (overlay-handle tracking-origin coordinates-in-overlay
                                          &key (overlay *overlay*))
  "Get the transform in 3d space associated with a specific 2d point in the overlay's coordinate space
   (where 0,0 is the lower left). -Z points out of the overlay."
  (cffi:with-foreign-objects ((coordinates '(:struct hmd-vector-2-t))
                              (matrix '(:struct hmd-matrix-34-t)))
    (setf (cffi:mem-ref coordinates '(:struct hmd-vector-2-t)) coordinates-in-overlay)
    (with-overlay-error
        (%get-transform-for-overlay-coordinates (table overlay) overlay-handle tracking-origin
                                                coordinates-in-overlay matrix))
    (cffi:mem-ref matrix '(:struct hmd-matrix-34-t))))

;; overlay input methods
(defun poll-next-overlay-event (overlay-handle &key (overlay *overlay*)))

(defun overlay-input-method (overlay-handle &key (overlay *overlay*))
  "Returns the current input settings for the specified overlay."
  (cffi:with-foreign-object (input-method 'vr-overlay-input-method)
    (with-overlay-error
        (%get-overlay-input-method (table overlay) overlay-handle input-method))
    (cffi:mem-ref input-method 'vr-overlay-input-method)))

(defun set-overlay-input-method (overlay-handle input-method &key (overlay *overlay*))
  "Sets the input settings for the specified overlay."
  (with-overlay-error
      (%set-overlay-input-method (table overlay) overlay-handle input-method)))

(defun overlay-mouse-scale (overlay-handle &key (overlay *overlay*))
  "Gets the mouse scaling factor that is used for mouse events. The actual texture may be a different size,
   but this is typically the size of the underlying UI in pixels."
  (cffi:with-foreign-object (foreign-mouse-scale '(:struct hmd-vector-2-t))
    (with-overlay-error (%get-overlay-mouse-scale (table overlay) overlay-handle foreign-mouse-scale))
    (cffi:mem-ref foreign-mouse-scale '(:struct hmd-vector-2-t))))

(defun set-overlay-mouse-scale (overlay-handle mouse-scale &key (overlay *overlay*))
  "Sets the mouse scaling factor that is used for mouse events. The actual texture may be a different size,
   but this is typically the size of the underlying UI in pixels (not in world space)."
  (cffi:with-foreign-object (pointer '(:struct hmd-vector-2-t))
    (setf (cffi:mem-ref pointer '(:struct hmd-vector-2-t)) mouse-scale)
    (with-overlay-error (%set-overlay-mouse-scale (table overlay) overlay-handle pointer))))

(defun compute-overlay-intersection (overlay-handle parameters &key (overlay *overlay*)))

@export
(defun hover-target-overlay-p (overlay-handle &key (overlay *overlay*))
  "Returns true if the specified overlay is the hover target. An overlay is the hover target when it is
   the last overlay 'moused over' by the virtual mouse pointer."
  (%is-hover-target-overlay (table overlay) overlay-handle))

@export
(defun set-overlay-dual-analog-transform (overlay-handle which center radius
                                          &key (overlay *overlay*))
  "Sets the analog input to Dual Analog coordinate scale for the specified overlay."
  (cffi:with-foreign-object (foreign-center '(:struct hmd-vector-2-t))
    (setf (cffi:mem-ref foreign-center '(:struct hmd-vector-2-t)) center)
    (with-overlay-error
      (%set-overlay-dual-analog-transform (table overlay) overlay-handle which foreign-center radius))))

@export
(defun overlay-dual-analog-transform (overlay-handle which &key (overlay *overlay*))
  "Gets the analog input to Dual Analog coordinate scale for the specified overlay."
  (cffi:with-foreign-objects ((foreign-center '(:struct hmd-vector-2-t))
                              (foreign-radius :float))
    (with-overlay-error
        (%get-overlay-dual-analog-transform (table overlay) overlay-handle which foreign-center foreign-radius))
    (values (cffi:mem-ref foreign-center '(:struct hmd-vector-2-t))
            (cffi:mem-ref foreign-radius :float))))

(defun set-overlay-intersection-mask (overlay-handle mask-primitives &key (overlay *overlay*)))

@export
(defun trigger-laser-mouse-haptic-vibration (overlay-handle duration-in-seconds frequency amplitude
                                             &key (overlay *overlay*))
  "Triggers a haptic event on the laser mouse controller for the specified overlay."
  (with-overlay-error
      (%trigger-laser-mouse-haptic-vibration (table overlay) overlay-handle
                                             duration-in-seconds frequency amplitude)))

@export
(defun set-overlay-cursor (overlay-handle cursor-handle &key (overlay *overlay*))
  "Sets the cursor to use for the specified overlay. This will be drawn instead of the generic blob when 
   the laser mouse is pointed at the specified overlay."
  (with-overlay-error (%set-overlay-cursor (table overlay) overlay-handle cursor-handle)))

(defun set-overlay-cursor-position-override (overlay-handle cursor &key (overlay *overlay*)))

@export
(defun clear-overlay-cursor-position-override (overlay-handle &key (overlay *overlay*))
  "Clears the override cursor position for this overlay."
  (with-overlay-error
      (%clear-overlay-cursor-position-override (table overlay) overlay-handle)))

;; overlay texture methods

@export
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

@export
(defun clear-overlay-texture (overlay-handle &key (overlay *overlay*))
  "Use this to tell the overlay system to release the texture set for this overlay."
  (with-overlay-error
      (%clear-overlay-texture (table overlay) overlay-handle)))

@export
(defun set-overlay-raw (overlay-handle buffer width height bytes-per-pixel
                        &key (overlay *overlay*))
  "Separate interface for providing the data as a stream of bytes, but there is an upper bound on data 
  that can be sent. This function can only be called by the overlay's renderer process. buffer must be a shareable
  byte vector."
  (cffi:with-pointer-to-vector-data (pointer buffer)
    (with-overlay-error
      (%set-overlay-raw (table overlay) overlay-handle pointer width height bytes-per-pixel))))

@export
(defun set-overlay-from-file (overlay-handle file-path &key (overlay *overlay*))
  "Separate interface for providing the image through a filename: can be png or jpg,
   and should not be bigger than 1920x1080.This function can only be called by the overlay's renderer process."
  (with-overlay-error
      (%set-overlay-from-file (table overlay) overlay-handle file-path)))

;;(defun overlay-texture (overlay-handle)) ;??
;;(defun release-native-overlay-handle) ;??

@export
(defun overlay-texture-size (overlay-handle &key (overlay *overlay*))
  (cffi:with-foreign-objects ((width :uint32)
                              (height :uint32))
    (with-overlay-error
        (%get-overlay-texture-size (table overlay) overlay-handle width height))
    (values (cffi:mem-ref width :uint32) (cffi:mem-ref height :uint32))))

;; dashboard overlay methods

@export
(defun create-dashboard-overlay (overlay-key overlay-friendly-name &key (overlay *overlay*))
  (cffi:with-foreign-objects ((main-pointer 'vr-overlay-handle-t)
                              (thumbnail-pointer 'vr-overlay-handle-t))
    (with-overlay-error
        (%create-dashboard-overlay (table overlay) overlay-key overlay-friendly-name
                                   main-pointer thumbnail-pointer))
    (values (cffi:mem-ref main-pointer 'vr-overlay-handle-t)
            (cffi:mem-ref thumbnail-pointer 'vr-overlay-handle-t))))

@export
(defun dashboard-visible-p (&key (overlay *overlay*))
  "Returns true if the dashboard is visible."
  (%is-dashboard-visible (table overlay)))

@export
(defun dashboard-overlay-active-p (overlay-handle &key (overlay *overlay*))
  (%is-active-dashboard-overlay (table overlay) overlay-handle))

@export
(defun set-dashboard-overlay-scene-process (overlay-handle process-id &key (overlay *overlay*))
  "Sets the dashboard overlay to only appear when the specified process ID has scene focus."
  (with-overlay-error
      (%set-dashboard-overlay-scene-process (table overlay) overlay-handle process-id)))

@export
(defun dashboard-overlay-scene-process (overlay-handle &key (overlay *overlay*))
  (cffi:with-foreign-object (pointer :uint32)
    (with-overlay-error
        (%get-dashboard-overlay-scene-process (table overlay) overlay-handle pointer))
    (cffi:mem-ref pointer :uint32)))

@export
(defun show-dashboard (overlay-to-show &key (overlay *overlay*))
  (%show-dashboard (table overlay) overlay-to-show))

@export
(defun primary-dashboard-device (&key (overlay *overlay*))
  "Returns the tracked device that has the laser pointer in the dashboard."
  (%get-primary-dashboard-device (table overlay)))

;; keyboard methods

@export
(defun show-keyboard (input-mode line-input-mode description max-char existing-text
                      use-minimal-mode-p user-value &key (overlay *overlay*))
  "Show the virtual keyboard to accept input."
  (with-overlay-error
      (%show-keyboard (table origin) input-mode line-input-mode description max-char existing-text
                      use-minimal-mode-p user-value)))

@export
(defun show-keyboard-for-overlay (overlay-handle input-mode line-input-mode description max-char
                                  existing-text use-minimal-mode-p user-value
                                  &key (overlay *overlay*))
  (with-overlay-error
      (%show-keyboard-for-overlay (table origin) overlay-handle input-mode line-input-mode description
                                  max-char existing-text use-minimal-mode-p user-value)))

@export
(defun keyboard-text (&key (overlay *overlay*) (buffer-size 512))
  "Get the text that was entered into the text input."
  (cffi:with-foreign-string (string-pointer (make-string buffer-size))
    (%get-keyboard-text (table overlay) string-pointer buffer-size)
    (cffi:foreign-string-to-lisp string-pointer)))

@export
(defun hide-keyboard (&key (overlay *overlay*))
  "Hide the virtual keyboard."
  (%hide-keyboard (table overlay)))

@export
(defun set-keyboard-transform-absolute (origin tracking-origin-to-keyboard-transform
                                        &key (overlay *overlay*))
  "Set the position of the keyboard in world space."
  (%set-keyboard-transform-absolute (table overlay) origin tracking-origin-to-keyboard-transform))  ; need a type translator?

@export
(defun set-keyboard-position-for-overlay (overlay-handle avoid-rect &key (overlay *overlay*))
  "Set the position of the keyboard in overlay space by telling it to avoid a rectangle in the overlay.
 Rectangle coords have (0,0) in the bottom left."
  (%set-keyboard-position-for-overlay (table overlay) overlay-handle avoid-rect)) ; need a type translator?

;; message box methods

@export
(defun show-message-overlay (text caption button-0-text button-1-text button-2-text button-3-text
                             &key (overlay *overlay*))
  "Show the message overlay. This will block and return you a result."
  (%show-message-overlay (table overlay) text caption button-0-text button-1-text button-2-text button-3-text))

@export
(defun close-message-overlay (&key (overlay *overlay*))
  "If the calling process owns the overlay and it's open, this will close it."
  (%close-message-overlay (table overlay)))

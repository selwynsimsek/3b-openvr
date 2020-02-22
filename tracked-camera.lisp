;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the TrackedCamera API.

;;; IVR_TrackedCamera_006

(in-package :3b-openvr)

(defmacro with-tracked-camera-error (&rest body)
  (let ((error-name (gensym "error-value")))
    `(let ((,error-name (progn ,@body)))
       (if (eq ,error-name :none) t (error "VR tracked-camera error: ~a" ,error-name)))))

(defun has-camera-p (tracked-device &key (tracked-camera *tracked-camera*))
  "For convenience, same as tracked property request Prop_HasCamera_Bool."
  (cffi:with-foreign-object (pointer :bool)
    (with-tracked-camera-error
        (%has-camera (table tracked-camera) tracked-device pointer))
    (cffi:mem-ref pointer :bool))) ; works

(defun camera-frame-size (tracked-device frame-type &key (tracked-camera *tracked-camera*))
  "Gets size of the image frame."
  (cffi:with-foreign-objects ((width :uint32)
                              (height :uint32)
                              (framebuffer-size :uint32))
    (%get-camera-frame-size
     (table tracked-camera) tracked-device frame-type width height framebuffer-size)
    (values (cffi:mem-ref width :uint32)
            (cffi:mem-ref height :uint32)
            (cffi:mem-ref framebuffer-size :uint32))))

(defun camera-intrinsics
    (tracked-device camera-index frame-type &key (tracked-camera *tracked-camera*))
  (cffi:with-foreign-objects ((focal-length '(:struct hmd-vector-2-t))
                              (center '(:struct hmd-vector-2-t)))
    (%get-camera-intrinsics
     (table tracked-camera) tracked-device camera-index frame-type focal-length center)
    (values (cffi:mem-ref focal-length '(:struct hmd-vector-2-t))
            (cffi:mem-ref center '(:struct hmd-vector-2-t))))) ; doesn't work

(defun camera-projection
    (tracked-device camera-index frame-type near far &key (tracked-camera *tracked-camera*))
  (cffi:with-foreign-object (pointer '(:struct hmd-matrix-44-t))
    (%get-camera-projection
     (table tracked-camera) tracked-device camera-index frame-type near far pointer)
    (cffi:mem-ref pointer '(:struct hmd-matrix-44-t))))

(defclass tracked-camera ()
  ((handle :initarg :handle :accessor handle)))

(defun acquire-video-streaming-service (tracked-device &key (tracked-camera *tracked-camera*))
  "Acquiring streaming service permits video streaming for the caller. Releasing hints the system 
   that video services do not need to be maintained for this client.
   If the camera has not already been activated, a one time spin up may incur some auto exposure as
   well as initial streaming frame delays.
   The camera should be considered a global resource accessible for shared consumption but not 
   exclusive to any caller.
   The camera may go inactive due to lack of active consumers or headset idleness."
  (cffi:with-foreign-object (pointer 'tracked-camera-handle-t)
    (%acquire-video-streaming-service (table tracked-camera) tracked-device pointer)
    (make-instance 'tracked-camera :handle (cffi:mem-ref pointer 'tracked-camera-handle-t))))

(defun release-video-streaming-service (camera &key (tracked-camera *tracked-camera*))
  (%release-video-streaming-service (table tracked-camera) camera))

(defun video-stream-frame-buffer-header (camera frame-type &key (tracked-camera *tracked-camera*))
  (cffi:with-foreign-object (pointer '(:struct camera-video-stream-frame-header-t))
    (%get-video-stream-frame-buffer
     (table tracked-camera) camera frame-type (cffi:null-pointer) 0 pointer
     (cffi:foreign-type-size '(:struct camera-video-stream-frame-header-t)))
    (cffi:mem-ref pointer '(:struct camera-video-stream-frame-header-t))))

(defun video-stream-frame-buffer (camera frame-type &key (tracked-camera *tracked-camera*))
  "The image data is currently provided as RGBA data, 4 bytes per pixel.
   If there is no frame available yet, due to initial camera spinup or re-activation, the error 
   will be :no-frame-available. Ideally a caller should be polling at ~16ms intervals."
  (let* ((header
           (video-stream-frame-buffer-header camera frame-type :tracked-camera tracked-camera))
         (shared-vector
           (cffi:make-shareable-byte-vector
            (* (width header) (height header) (bytes-per-pixel header))))))
  (cffi:with-pointer-to-vector-data (pointer shared-vector)
    (%get-video-stream-frame-buffer
     (table tracked-camera) camera frame-type pointer (length pointer) (cffi:null-pointer) 0)))

(defun video-stream-texture-size (tracked-device frame-type &key (tracked-camera *tracked-camera*))
  "Gets size of the image frame."
  (cffi:with-foreign-objects ((texture '(:struct vr-texture-bounds-t))
                              (width :uint32)
                              (height :uint32))
    (%get-video-stream-texture-size
     (table tracked-camera) tracked-device frame-type texture width height)
    (values (cffi:mem-ref texture '(:struct vr-texture-bounds-t))
            (cffi:mem-ref width :uint32)
            (cffi:mem-ref height :uint32))))

(defun video-stream-texture-gl
    (camera frame-type &key (tracked-camera *tracked-camera*))
  "Access a shared GL texture for the specified tracked camera stream."
  (cffi:with-foreign-objects ((gl-pointer 'gl-uint-t)
                              (frame-pointer '(:struct camera-video-stream-frame-header-t)))
    (%get-video-stream-texture-gl
     (table tracked-camera) camera frame-type gl-pointer frame-pointer
     (cffi:foreign-type-size '(:struct camera-video-stream-frame-header-t)))
    (values (cffi:mem-ref gl-pointer 'gl-uint-t)
            (cffi:mem-ref frame-pointer '(:struct camera-video-stream-frame-header-t)))))

(defun release-video-stream-texture-gl (camera texture-id &key (tracked-camera *tracked-camera*))
  "Release a shared GL texture for the specified tracked camera stream."
  (%release-video-stream-texture-gl (table tracked-camera) camera texture-id))

(defun set-camera-tracking-space (tracking-universe-origin &key (tracked-camera *tracked-camera*))
  (%set-camera-tracking-space (table tracked-camera) tracking-universe-origin)) ; works

(defun camera-tracking-space (&key (tracked-camera *tracked-camera*))
  (%get-camera-tracking-space (table tracked-camera))) ; works

(define-clos-wrapper (video-stream-frame-header camera-video-stream-frame-header-t) ()
                     ((frame-type frame-type)
                      (width width)
                      (height height)
                      (bytes-per-pixel bytes-per-pixel)
                      (frame-sequence frame-sequence)
                      (tracked-device-pose tracked-device-pose)
                      (frame-exposure-time frame-exposure-time)))

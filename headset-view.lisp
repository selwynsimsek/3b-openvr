;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level API for the IVRHeadsetView API 

;;; IVRHeadsetView_001

(in-package :3b-openvr)

(defun set-headset-view-size (width height &key (headset-view *headset-view*))
  "Sets the resolution in pixels to render the headset view. These values are clamped to +headset-view-max-width+ and
   +headset-view-max-height+ respectively. For cropped views, the rendered output will be fit to aspect ratio defined by
   the the specified dimensions. For uncropped views, the caller should use #'headset-view-aspect-raio to adjust the
   requested render size to avoid squashing or stretching, and then apply letterboxing to compensate when displaying 
   the results."
  (%set-headset-view-size (table headset-view) width height))


(defun headset-view-size (&key (headset-view *headset-view*))
  "Gets the current resolution used to render the headset view."
  (cffi:with-foreign-objects ((width-pointer :uint32)
                              (height-pointer :uint32))
    (%get-headset-view-size (table headset-view) width-pointer height-pointer)
    (values (cffi:mem-ref width-pointer :uint32) (cffi:mem-ref height-pointer :uint32))))


(defun set-headset-view-mode (headset-view-mode &key (headset-view *headset-view*))
  "Set the mode used to render the headset view."
  (%set-headset-view-mode (table headset-view) headset-view-mode))


(defun headset-view-mode (&key (headset-view *headset-view*))
  "Get the current mode used to render the headset view."
  (%get-headset-view-mode (table headset-view)))


(defun set-headset-view-cropped (cropped-p &key (headset-view *headset-view*))
  "Set whether or not the headset view should be rendered cropped to hide the hidden area mesh or not."
  (%set-headset-view-cropped (table headset-view) cropped-p))


(defun headset-view-cropped-p (&key (headset-view *headset-view*))
  "Get the current cropping status of the headset view."
  (%get-headset-view-cropped (table headset-view)))


(defun headset-view-aspect-ratio (&key (headset-view *headset-view*))
  "Get the aspect ratio (width:height) of the uncropped headset view (accounting for the current set mode)."
  (%get-headset-view-aspect-ratio (table headset-view)))


(defun set-headset-view-blend-range (start end &key (headset-view *headset-view*))
  "Set the range [0..1] that the headset view blends across the stereo overlapped area in cropped both mode."
  (%set-headset-view-blend-range (table headset-view) start end))


(defun headset-view-blend-range (&key (headset-view *headset-view*))
  "Get the current range [0..1] that the headset view blends across the stereo overlapped area in cropped both mode."
  (cffi:with-foreign-objects ((width-pointer :float)
                              (height-pointer :float))
    (%get-headset-view-blend-range (table headset-view) width-pointer height-pointer)
    (values (cffi:mem-ref width-pointer :float) (cffi:mem-ref height-pointer :float))))

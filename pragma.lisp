;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CFFI redefinitions to cope with the differing alignments of some types on linux/macOS.

;;; Some structs are packed differently on Linux and Darwin, which these cffi redefinitions take care
;;; of. Correct for 1.9.15. Consult openvr_capi.h for details.

(in-package :3b-openvr)

#+(or darwin linux)
(cffi:defcstruct render-model-texture-map-t
  (width :uint16)
  (height :uint16 :offset 4)
  (texture-map-data (:pointer :uint8) :offset 8))

#+(or darwin linux)
(cffi:defcstruct (render-model-t :size 32)
  (vertex-data (:pointer (:struct render-model-vertex-t))) ;; const struct vr::RenderModel_Vertex_t *
  (vertex-count :uint32 :offset 8) ;; uint32_t
  (index-data (:pointer :uint16) :offset 12) ;; const uint16_t *
  (triangle-count :uint32 :offset 20) ;; uint32_t
  (diffuse-texture-id texture-id-t :offset 28)) ;; TextureID_t

#+(or darwin linux)
(defcstruct vr-event-t
  (event-type :uint32) ;; uint32_t
  (tracked-device-index tracked-device-index-t :offset 4) ;; TrackedDeviceIndex_t
  (event-age-seconds :float :offset 8) ;; float
  (data (:union vr-event-data-t) :offset 16)) ;; VREvent_Data_t

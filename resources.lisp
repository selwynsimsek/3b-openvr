;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the IVRResources API.

;;; IVR_Resources_001

(in-package 3b-openvr)

(defun load-shared-resource (name &key (resources *resources*))
  "Loads the specified resource into a byte-vector.")

(defun resource-pathname (resource-name resource-type-directory &key (resources *resources*))
  "Provides the full path to the specified resource.
  Resource names can include named directories for drivers and other things,
  and this resolves all of those and returns the actual physical path.
  resource-type-directory is the subdirectory of resources to look in."
  )

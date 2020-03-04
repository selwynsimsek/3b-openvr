;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the IVRResources API.

;;; IVR_Resources_001

(in-package 3b-openvr)

(annot:enable-annot-syntax)

@export
(defun load-shared-resource (name &key (resources *resources*))
  "Loads the specified resource into a shareable byte vector."
  (let* ((buffer-size (%load-shared-resource (table resources) name (cffi:null-pointer) 0))
         (buffer-array (cffi:make-shareable-byte-vector (list buffer-size))))
    (cffi:with-pointer-to-vector-data (pointer buffer-array)
      (%load-shared-resource (table resources) name pointer buffer-size))
     buffer-array)) ; works

@export
(defun resource-pathname (resource-name resource-type-directory &key (resources *resources*)
                                                                     (buffer-size 512))
  "Provides the full path to the specified resource. Resource names can include named directories 
  for drivers and other things, and this resolves all of those and returns the actual physical path.
  resource-type-directory is the subdirectory of resources to look in."
  (cffi:with-foreign-string (foreign-string (make-string buffer-size))
    (%get-resource-full-path (table resources) resource-name resource-type-directory foreign-string buffer-size)
    (cffi:foreign-string-to-lisp foreign-string))) ; works

(in-package 3b-openvr)

(cffi:define-foreign-library openvr-api
  (:windows #.(asdf:system-relative-pathname '3b-openvr "lib/win64/openvr_api.dll"))
  (:linux #.(asdf:system-relative-pathname '3b-openvr "lib/linux64/libopenvr_api.so"))
  (:darwin #.(asdf:system-relative-pathname '3b-openvr"lib/osx32/libopenvr_api.dylib")))

(cffi:use-foreign-library openvr-api)

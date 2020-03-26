(defsystem :3b-openvr
  :description "Common Lisp bindings for the OpenVR API"
  :depends-on (cffi-libffi alexandria trivial-features cl-json str)
  :serial t
  :bug-tracker "https://github.com/selwynsimsek/3b-openvr/issues"
  :source-control (:git "https://github.com/selwynsimsek/3b-openvr.git")
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components ((:file "package")
               (:file "low-level")
               (:file "wrappers")
               (:file "pragma")
               (:file "library")
               (:file "clos")
               (:file "system")
               (:file "chaperone")
               (:file "chaperone-setup")
               (:file "compositor")
               (:file "overlay")
               (:file "resources")
               (:file "render-models")
               (:file "extended-display")
               (:file "settings")
               (:file "applications")
               (:file "tracked-camera")
               (:file "screenshots")
               (:file "driver-manager")
               (:file "input")
               (:file "io-buffer")
               (:file "spatial-anchors")
               (:file "debug")
               (:file "notifications")))

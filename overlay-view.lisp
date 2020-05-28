

(in-package :3b-openvr)

(defun viewing-permitted-p (overlay-handle &key (overlay-view *overlay-view*))
  (%is-viewing-permitted (table overlay-view) overlay-handle))

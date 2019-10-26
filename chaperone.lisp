; version = IVR_Chaperone_003

(defun calibration-state (&key (chaperone *chaperone*))
  (%get-calibration-state (table chaperone)))

(defun play-area-size (&key (chaperone *chaperone*)))

(defun play-area-rect (&key (chaperone *chaperone*)))

(defun reload-chaperone-info (&key (chaperone *chaperone*))
  (%reload-info (table chaperone)))

(defun set-scene-color (color &key (chaperone *chaperone*)))

(defun bounds-color (collision-bounds-fade-distance &key (chaperone *chaperone*)))

(defun bounds-visible-p (&key (chaperone *chaperone*))
  (%are-bounds-visible (table chaperone)))

(defun force-bounds-visible (bounds-visible-p &key (chaperone *chaperone*))
  (%force-bounds-visible (table chaperone) bounds-visible-p))

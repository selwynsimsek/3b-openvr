; version = IVR_System_020

(in-package 3b-openvr)

(defun poll-next-event (&key (system *system*))
  (cffi:with-foreign-object (ev '(:struct vr-event-t))
    (when (%poll-next-event (table system) ev (cffi:foreign-type-size
                                               '(:struct vr-event-t)))
      (let ((R (multiple-value-list
                (ignore-errors
                 (cffi:mem-ref ev '(:struct vr-event-t))))))
        (when (second r)
          (format t "~&~a?~%" (second r))
          (loop for i below 40
                do (format t " #~2,'0x" (cffi:mem-aref ev :uint8 i))
                when (zerop (mod (1+ i) 8))
                do (format t "~%")))
        (first r)))))

(defun get-controller-state (device &key (system *system*))
  (cffi:with-foreign-object (state '(:struct vr-controller-state-001-t))
    (when (%get-controller-state (table system) device state
                                 (cffi:foreign-type-size
                                  '(:struct vr-controller-state-001-t)))
      (cffi:mem-ref state '(:struct vr-controller-state-001-t)))))

(defun is-tracked-device-connected (index &key (system *system*))
  (%is-tracked-device-connected (table system) index))

(defun get-controller-role-for-tracked-device-index (index &key (system *system*))
  (%get-controller-role-for-tracked-device-index (table system) index))

(defun get-tracked-device-index-for-controller-role (role &key (system *system*))
  (let ((i (%get-tracked-device-index-for-controller-role (table system) role)))
    (if (= i #xffffffff)
        nil
        i)))

(defun get-tracked-device-class (index &key (system *system*))
  (%get-tracked-device-class (table system) index))

(defun get-recommended-render-target-size (&key (system *system*))
  (cffi:with-foreign-objects ((w :uint32)
                              (h :uint32))
    (%get-recommended-render-target-size (table system) w h)
    ;; should this return list, values, or some struct/class or something?
    (values (cffi:mem-ref w :uint32) (cffi:mem-ref h :uint32))))

(defun get-projection-matrix (eye near far &key (system *system*))
  (%get-projection-matrix (table system) eye near far))

(defun get-eye-to-head-transform (eye &key (system *system*))
  (%get-eye-to-head-transform (table system) eye))

(defun is-input-focus-captured-by-another-process (&key (system *system*))
  (%is-input-focus-captured-by-another-process (table system)))

(defun is-input-available (&key (system *system*))
  (%is-input-available (table system)))

(defun get-string-tracked-device-property (device-index prop &key (system *system*))
  (with-error (pe tracked-property-error)
    (let ((len (%get-string-tracked-device-property (table system)
                                                    device-index prop
                                                    (cffi:null-pointer) 0
                                                    pe)))
      (unless (eql (cffi:mem-ref pe 'tracked-property-error) :buffer-too-small)
        ;; buffer-too-small is expected here?
        (pe :val prop))
      (if (zerop len)
          ""
          (cffi:with-foreign-pointer-as-string (s (+ 11 len))
            (%get-string-tracked-device-property (table system)
                                                 device-index prop
                                                 s (+ 11 len) pe)
            (pe :val prop))))))

(defun get-float-tracked-device-property (device-index prop &key (system *system*))
  (with-error (pe tracked-property-error)
    (let ((r (%get-float-tracked-device-property (table system)
                                                 device-index prop
                                                 pe)))
      (pe :val prop)
      r)))

(defun get-bool-tracked-device-property (device-index prop &key (system *system*))
  (with-error (pe tracked-property-error)
    (let ((r (%get-bool-tracked-device-property (table system)
                                                device-index prop
                                                pe)))
      (pe :val prop)
      r)))

(defun get-int32-tracked-device-property (device-index prop &key (system *system*))
  (with-error (pe tracked-property-error)
    (let ((r (%get-int32-tracked-device-property (table system)
                                                 device-index prop
                                                 pe)))
      (pe :val prop)
      r)))

(defun get-uint64-tracked-device-property (device-index prop &key (system *system*))
  (with-error (pe tracked-property-error)
    (let ((r (%get-uint64-tracked-device-property (table system)
                                                  device-index prop
                                                  pe)))
      (pe :val prop)
      r)))

(defun get-matrix-34-tracked-device-property (device-index prop &key (system *system*))
  (with-error (pe tracked-property-error)
    (cffi:with-foreign-object (fp :float 12)
      (let ((r (%get-matrix-34-tracked-device-property (table system)
                                                       device-index prop
                                                       pe)))
        ()
        (pe :val prop)
        r))))

(defun get-tracked-device-property (device-index prop &key (system *system*))
  ;; todo: make a hash table of types or something instead of string matching
  (unless (member prop '(:invalid))
    (let ((sprop (string prop)))
      (cond
        ((alexandria:ends-with-subseq "-STRING" sprop)
         (get-string-tracked-device-property device-index prop :system system))
        ((alexandria:ends-with-subseq "-INT32" sprop)
         (get-int32-tracked-device-property device-index prop :system system))
        ((alexandria:ends-with-subseq "-UINT64" sprop)
         (get-uint64-tracked-device-property device-index prop :system system))
        ((alexandria:ends-with-subseq "-BOOL" sprop)
         (get-bool-tracked-device-property device-index prop :system system))
        ((alexandria:ends-with-subseq "-FLOAT" sprop)
         (get-float-tracked-device-property device-index prop :system system))
        ((alexandria:ends-with-subseq "-MATRIX-34" sprop)
         (get-matrix-34-tracked-device-property device-index prop :system system))
        ((or (alexandria:ends-with-subseq "-START" sprop)
             (alexandria:ends-with-subseq "-END" sprop))
         nil)
        (t (error "unknown prop type ~s?" prop))))))

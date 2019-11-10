(in-package 3b-openvr)

(defvar *%init*)
(defvar *system*)
(defvar *chaperone*)
(defvar *chaperone-setup*)
(defvar *compositor*)
(defvar *overlay*)
(defvar *resources*)
(defvar *render-models*)
(defvar *extended-display*)
(defvar *settings*)
(defvar *applications*)
(defvar *tracked-camera*)
(defvar *screenshots*)
(defvar *driver-manager*)
(defvar *input*)
(defvar *io-buffer*)
(defvar *spatial-anchors*)
(defvar *debug*)
(defvar *notifications*)

(defmethod cffi:translate-from-foreign (value (type hmd-matrix-34-t-tclass))
  (let ((a (make-array 16 :element-type 'single-float :initial-element 0.0)))
    (loop for j below 4
          do (loop for i below 3
                   do (setf (aref a (+ (* j 4) i))
                            (cffi:mem-aref value :float (+ (* i 4) j)))))
    (setf (aref a 15) 1.0)
    a))

(defmethod cffi:translate-from-foreign (value (type hmd-matrix-44-t-tclass))
  (let ((a (make-array 16 :element-type 'single-float :initial-element 0.0)))
    (dotimes (j 4)
      (dotimes (i 4)
        (setf (aref a (+ i (* j 4))) (cffi:mem-aref value :float (+ j (* i 4))))))
    a))

(defmethod cffi:translate-from-foreign (value (type hmd-quaternionf-t-tclass))
  (cffi:with-foreign-slots ((w x y z) value (:struct hmd-quaternionf-t))
    (vector w x y z)))

(defmethod cffi:translate-from-foreign (value (type hmd-quaternion-t-tclass))
  (cffi:with-foreign-slots ((w x y z) value (:struct hmd-quaternion-t))
    (vector (coerce w 'single-float)
            (coerce x 'single-float)
            (coerce y 'single-float)
            (coerce z 'single-float))))

(defmethod cffi:translate-from-foreign (value (type hmd-vector-3-t-tclass))
  (let ((a (make-array '(3) :initial-element 0.0)))
    (setf (aref a 0) (cffi:mem-aref value :float 0)
          (aref a 1) (cffi:mem-aref value :float 1)
          (aref a 2) (cffi:mem-aref value :float 2))
    a))

;;; fixme: replace this with map to functions instead of types, so
;;; they don't need runtime dispatch in cffi
(defparameter *event-type-map* ;; indexed by event-type
  ;; todo: verify + figure out more of these
  (alexandria:plist-hash-table
   '(:none nil
     :tracked-device-activated nil
     :tracked-device-deactivated nil
     :tracked-device-updated nil
     :tracked-device-user-interaction-started nil
     :tracked-device-user-interaction-ended nil
     :ipd-changed nil
     :enter-standby-mode nil
     :leave-standby-mode nil
     :tracked-device-role-changed nil
     :watchdog-wake-up-requested nil
     :lens-distortion-changed nil
     :property-changed nil
     :wireless-disconnect nil
     :wireless-reconnect nil
     :button-press (:struct vr-event-controller-t)
     :button-unpress (:struct vr-event-controller-t)
     :button-touch (:struct vr-event-controller-t)
     :button-untouch (:struct vr-event-controller-t)
     :mouse-move (:struct vr-event-mouse-t)
     :mouse-button-down (:struct vr-event-mouse-t)
     :mouse-button-up (:struct vr-event-mouse-t)
     :focus-enter (:struct vr-event-overlay-t)
     :focus-leave (:struct vr-event-overlay-t)
     :scroll (:struct vr-event-mouse-t)
     :touch-pad-move (:struct vr-event-mouse-t)
     :overlay-focus-changed (:struct vr-event-process-t)
     :input-focus-captured (:struct vr-event-process-t)
     :input-focus-released (:struct vr-event-process-t)
     :scene-focus-lost (:struct vr-event-process-t)
     :scene-focus-gained (:struct vr-event-process-t)
     :scene-application-changed (:struct vr-event-process-t)
     :scene-focus-changed (:struct vr-event-process-t)
     :input-focus-changed (:struct vr-event-process-t)
     :scene-application-secondary-rendering-started (:struct vr-event-process-t)
     :hide-render-models nil
     :show-render-models nil
     :overlay-shown nil
     :overlay-hidden nil
     :dashboard-activated nil
     :dashboard-deactivated nil
     :dashboard-thumb-selected (:struct vr-event-overlay-t)
     :dashboard-requested (:struct vr-event-overlay-t)
     :reset-dashboard nil
     :render-toast (:struct vr-event-notification-t) ;;?
     :image-loaded nil
     :show-keyboard nil
     :hide-keyboard nil
     :overlay-gamepad-focus-gained nil
     :overlay-gamepad-focus-lost nil
     :overlay-shared-texture-changed nil
     :dashboard-guide-button-down nil
     :dashboard-guide-button-up nil
     :screenshot-triggered nil
     :image-failed nil
     :dashboard-overlay-created nil
     :request-screenshot nil
     :screenshot-taken nil
     :screenshot-failed nil
     :submit-screenshot-to-dashboard nil
     :screenshot-progress-to-dashboard nil
     :primary-dashboard-device-changed nil
     :notification-shown nil
     :notification-hidden nil
     :notification-begin-interaction nil
     :notification-destroyed nil
     :quit (:struct vr-event-process-t)
     :process-quit (:struct vr-event-process-t)
     :quit-aborted-user-prompt (:struct vr-event-process-t)
     :quit-acknowledged (:struct vr-event-process-t)
     :driver-requested-quit nil
     :chaperone-data-has-changed nil
     :chaperone-universe-has-changed nil
     :chaperone-temp-data-has-changed nil
     :chaperone-settings-have-changed nil
     :seated-zero-pose-reset nil
     :audio-settings-have-changed nil
     :background-setting-has-changed nil
     :camera-settings-have-changed nil
     :reprojection-setting-has-changed nil
     :model-skin-settings-have-changed nil
     :environment-settings-have-changed nil
     :power-settings-have-changed nil
     :enable-home-app-settings-have-changed nil
     :status-update nil
     :mcimage-updated nil
     :firmware-update-started nil
     :firmware-update-finished nil
     :keyboard-closed nil
     :keyboard-char-input nil
     :keyboard-done nil
     :application-transition-started nil
     :application-transition-aborted nil
     :application-transition-new-app-started nil
     :application-list-updated nil
     :application-mime-type-load nil
     :application-transition-new-app-launch-complete nil
     :process-connected nil
     :process-disconnected nil
     :compositor-mirror-window-shown nil
     :compositor-mirror-window-hidden nil
     :compositor-chaperone-bounds-shown nil
     :compositor-chaperone-bounds-hidden nil
     :tracked-camera-start-video-stream nil
     :tracked-camera-stop-video-stream nil
     :tracked-camera-pause-video-stream nil
     :tracked-camera-resume-video-stream nil
     :tracked-camera-editing-surface nil
     :performance-test-enable-capture nil
     :performance-test-disable-capture nil
     :performance-test-fidelity-level nil
     :message-overlay-closed nil
     :message-overlay-close-requested nil
     :vendor-specific-reserved-start nil
     :vendor-specific-reserved-end nil)))

(defclass vr-event ()
  ((event-type :initarg :event-type :accessor event-type)
   (tracked-device-index :initarg :tracked-device-index :accessor tracked-device-index)
   (event-age :initarg :event-age :accessor event-age)
   (data :initarg :data :accessor data)))

(defmethod cffi:translate-from-foreign (value (type vr-event-t-tclass))
  (let* ((type (cffi:foreign-enum-keyword
                'vr-event-type
                (cffi:foreign-slot-value value '(:struct vr-event-t)
                                         'event-type)))
         (union (gethash type *event-type-map*))
         (tracked-device-index (cffi:foreign-slot-value value
                                                        '(:struct vr-event-t)
                                                        'tracked-device-index))
         (event-age-seconds (cffi:foreign-slot-value value
                                                     '(:struct vr-event-t)
                                                     'event-age-seconds))
         (offset (cffi:foreign-slot-offset '(:struct vr-event-t) 'data))
         (size (cffi:foreign-type-size '(:struct vr-event-t)))
         (data (if union
                   (cffi:mem-ref
                    (cffi:foreign-slot-pointer value '(:struct vr-event-t)
                                               'data)
                    union)
                   (coerce
                    (loop for i from offset below size
                          collect (cffi:mem-aref value :uint8 i))
                    '(simple-array (unsigned-byte 8) (*))))))
    (make-instance 'vr-event :event-type type
                             :tracked-device-index tracked-device-index
                             :event-age event-age-seconds
                             :data data)))

(cffi:defcfun (%vr-init-internal "VR_InitInternal") :intptr
  (pe-error (:pointer vr-init-error))
  (type vr-application-type))

(cffi:defcfun (%vr-init-internal2 "VR_InitInternal2") :intptr
  (pe-error (:pointer vr-init-error))
  (type vr-application-type)
  (startup-info :string))

(cffi:defcfun (vr-shutdown-internal "VR_ShutdownInternal") :void)

(cffi:defcfun (vr-is-hmd-present "VR_IsHmdPresent") :bool)

(cffi:defcfun (vr-get-init-token "VR_GetInitToken") :uint32)


(cffi:defcfun (%vr-get-generic-interface "VR_GetGenericInterface") :intptr
  (pch-Interface-Version :string)
  (pe-error (:pointer vr-init-error)))

(cffi:defcfun (vr-is-runtime-installed "VR_IsRuntimeInstalled") :bool)

(cffi:defcfun (vr-get-init-error-as-symbol "VR_GetVRInitErrorAsSymbol") :string
  (error vr-init-error))

(cffi:defcfun (vr-get-init-error-as-english-description
               "VR_GetVRInitErrorAsEnglishDescription") :string
  (error vr-init-error))

(alexandria:define-constant +function-table-prefix+ "FnTable:" :test 'string=)

(defun vr-init (application-type)
  (cffi:with-foreign-object (pe 'vr-init-error)
    (let ((r (%vr-init-internal pe application-type))
          (e (cffi:mem-ref pe 'vr-init-error)))
      (if (eql e :none)
          r
          (error "VR-init error (~s): ~s = ~a = ~a~%" application-type r
                 (vr-get-init-error-as-english-description e)
                 (vr-get-init-error-as-symbol e))))))

(defun vr-get-generic-interface (name &key (table t))
  (cffi:with-foreign-object (pe 'vr-init-error)
    (let* ((name (if table
                     (format nil "~a~a" +function-table-prefix+ name)
                     name))
           (r (%vr-get-generic-interface name pe))
           (e (cffi:mem-ref pe 'vr-init-error)))
      (if (eql e :none)
          r
          (error "failed to get ~s interface: ~s = ~a = ~a~%"
                 name r
                 (vr-get-init-error-as-english-description e)
                 (vr-get-init-error-as-symbol e)))
      (format t " = #x~x~%" r)
      (cffi:make-pointer r))))

(defun clear ()
  (setf *system* nil)
  (setf *chaperone* nil)
  (setf *chaperone-setup* nil)
  (setf *compositor* nil)
  (setf *overlay* nil)
  (setf *render-models* nil)
  (setf *extended-display* nil)
  (setf *settings* nil)
  (setf *applications* nil)
  (setf *tracked-camera* nil)
  (setf *resources* nil)
  (setf *screenshots* nil))

(defun check-clear ()
  (unless (eql *%init* (vr-get-init-token))
    (clear)
    (setf *%init* (vr-get-init-token))))

(defun vr-system ()
  (check-clear)
  (unless *system*
    (setf *system* (make-instance 'vr-system)))
  *system*)

(defun vr-input ()
  (check-clear)
  (unless *input*
    (setf *input* (make-instance 'vr-input)))
  *input*)

(defun vr-chaperone ()
  (check-clear)
  (unless *chaperone*
    (setf *chaperone* (make-instance 'vr-chaperone)))
  *chaperone*)

(defun vr-chaperone-setup ()
  (check-clear)
  (unless *chaperone-setup*
    (setf *chaperone-setup* (make-instance 'vr-chaperone-setup)))
  *chaperone-setup*)

(defun vr-compositor ()
  (check-clear)
  (unless *compositor*
    (setf *compositor* (make-instance 'vr-compositor)))
  *compositor*)

(defun vr-overlay ()
  (check-clear)
  (unless *overlay*
    (setf *overlay* (make-instance 'vr-overlay)))
  *overlay*)

(defun vr-resources ()
  (check-clear)
  (unless *overlay*
    (setf *overlay* (make-instance 'vr-resources)))
  *overlay*)

(defun vr-render-models ()
  (check-clear)
  (unless *render-models*
    (setf *render-models* (make-instance 'vr-render-models)))
  *render-models*)

(defun vr-extended-display ()
  (check-clear)
  (unless *extended-display*
    (setf *extended-display* (make-instance 'vr-extended-display)))
  *extended-display*)

(defun vr-settings ()
  (check-clear)
  (unless *settings*
    (setf *settings* (make-instance 'vr-settings)))
  *settings*)

(defun vr-applications ()
  (check-clear)
  (unless *applications*
    (setf *applications* (make-instance 'vr-applications)))
  *applications*)

(defun vr-tracked-camera ()
  (check-clear)
  (unless *tracked-camera*
    (setf *tracked-camera* (make-instance 'vr-tracked-camera)))
  *tracked-camera*)

(defun vr-resources ()
  (check-clear)
  (unless *resources*
    (setf *resources* (make-instance 'vr-resources)))
  *resources*)

(defun vr-screenshots ()
  (check-clear)
  (unless *screenshots*
    (setf *screenshots* (make-instance 'vr-screenshots)))
  *screenshots*)

(defun vr-driver-manager ()
  (check-clear)
  (unless *driver-manager*
    (setf *driver-manager* (make-instance 'vr-driver-manager)))
  *driver-manager*)

(defun vr-input ()
  (check-clear)
  (unless *input*
    (setf *input* (make-instance 'vr-input)))
  *input*)

(defun vr-io-buffer ()
  (check-clear)
  (unless *io-buffer*
    (setf *io-buffer* (make-instance 'vr-iobuffer)))
  *io-buffer*)

(defun vr-spatial-anchors ()
  (check-clear)
  (unless *spatial-anchors*
    (setf *spatial-anchors* (make-instance 'vr-spatial-anchors)))
  *spatial-anchors*)

(defun vr-debug ()
  (check-clear)
  (unless *debug*
    (setf *debug* (make-instance 'vr-debug)))
  *debug*)

(defun vr-notifications ()
  (check-clear)
  (unless *notifications*
    (setf *notifications* (make-instance 'vr-notifications)))
  *notifications*)

(defmacro with-vr ((&key (application-type :scene)) &body body)
  `(let ((*%init* (vr-init ,application-type))
         (*system* nil)
         (*chaperone* nil)
         (*chaperone-setup* nil)
         (*compositor* nil)
         (*overlay* nil)
         (*resources* nil)
         (*render-models* nil)
         (*extended-display* nil)
         (*settings* nil)
         (*applications* nil)
         (*tracked-camera* nil)
         (*screenshots* nil)
         (*driver-manager* nil)
         (*input* nil)
         (*io-buffer* nil)
         (*spatial-anchors* nil)
         (*debug* nil)
         (*notifications* nil))
     (unwind-protect
          (progn (vr-system)
                 (vr-chaperone)
                 (vr-chaperone-setup)
                 (vr-compositor)
                 (vr-overlay)
                 (vr-resources)
                 (vr-render-models)
                 (vr-extended-display)
                 (vr-settings)
                 (vr-applications)
                 (vr-tracked-camera)
                 (vr-screenshots)
                 (vr-driver-manager)
                 (vr-input)
                 (vr-io-buffer)
                 (vr-spatial-anchors)
                 (vr-debug)
                 (vr-notifications)
                 ,@body)
       (vr-shutdown-internal))))

#++
(defmacro check-error (pointer type)
  `(unless (eql (cffi:mem-ref ,pointer ',type) :none)
     (error "error ~s (~s)" (cffi:mem-ref ,pointer ',type) ',type)))
#++
(defmethod get-string-tracked-device-property ((o vr-system) device-index prop)
  (cffi:with-foreign-object (pe 'tracked-property-error)
    (let ((len (%get-string-tracked-device-property (table o) device-index prop
                                                    (cffi:null-pointer) 0
                                                    pe)))
      (check-error pe tracked-property-error)
      (cffi:with-foreign-pointer-as-string (s (1+ len))
        (%get-string-tracked-device-property (table o) device-index prop
                                             s len pe)
        (check-error pe tracked-property-error)))))

(defmacro with-error ((pointer type) &body body)
  `(cffi:with-foreign-object (pe 'tracked-property-error)
     ;; option to pass recursive format string + args instead of just :val?
     (flet ((,pointer (&key val)
              (unless (member (cffi:mem-ref ,pointer ',type)
                              '(:none :success))
                (error "~(~s: ~a~) ~@[(~s)~]"
                       ',type
                       (cffi:mem-ref ,pointer ',type)
                       val))))
       ,@body)))

(defmacro check-ret (call &key (ok '(:success :none)))
  `(let ((r ,call))
     (unless (member r ',ok)
       (error "~a failed: ~a" ',(car call) r))
     r))

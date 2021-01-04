;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CFFI translators for VREvent_t.

;;; Here we do function-based dispatch to do the type translation as 3b suggested. This is done in
;;; order to avoid cffi's slower runtime dispatch.

(in-package 3b-openvr)

(defclass vr-event ()
  ((event-type :initarg :event-type :accessor event-type)
   (tracked-device-index :initarg :tracked-device-index :accessor tracked-device-index)
   (event-age :initarg :event-age :accessor event-age)))

(defun make-bare-event (event-type tracked-device-index data-pointer event-age-seconds)
  (declare (ignore data-pointer))
  (make-instance 'vr-event :event-type event-type :tracked-device-index tracked-device-index
                                 :event-age event-age-seconds))

(defclass reserved-event (vr-event)
  ((reserved-data-array :initarg :reserved-data-array :accessor reserved-data-array)))

(defun make-reserved-event (event-type tracked-device-index data-pointer event-age-seconds)
  (make-instance 'reserved-event :event-type event-type :tracked-device-index tracked-device-index
                                 :event-age event-age-seconds :reserved-data-array
                                 (cffi:foreign-array-to-lisp data-pointer
                                                             '(:array :uint64 6))))

(defclass controller-event (vr-event)
  ((button-id :initarg :button-id :accessor button-id)))

(defun make-controller-event (event-type tracked-device-index data-pointer event-age-seconds)
  (make-instance 'controller-event :button-id (cffi:mem-ref data-pointer :uint32)
                                   :event-age event-age-seconds
                                   :tracked-device-index tracked-device-index
                                   :event-type event-type))

(defclass mouse-event (vr-event)
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (button-id :initarg :button-id :accessor button-id)))

(defun make-mouse-event (event-type tracked-device-index data-pointer event-age-seconds)
  (make-instance 'mouse-event
                 :x (cffi:mem-ref data-pointer :float)
                 :y (cffi:mem-ref data-pointer :float 4)
                 :button-id (cffi:mem-ref data-pointer :float 8)
                 :event-age event-age-seconds
                 :tracked-device-index tracked-device-index
                 :event-type event-type))

(defclass scroll-event (vr-event)
  ((x-delta :initarg :x-delta :accessor x-delta)
   (y-delta :initarg :y-delta :accessor y-delta)
   (unused :initarg :unused :accessor unused)
   (viewport-scale :initarg :viewport-scale :accessor viewport-scale)))

(defun make-scroll-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass process-event (vr-event)
  ((pid :initarg :x-delta :accessor pid)
   (old-pid :initarg :y-delta :accessor old-pid)
   (forced-p :initarg :forced-p :accessor forced-p)
   (connection-lost-p :initarg :connection-lost-p :accessor connection-lost-p)))

(defun make-process-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass notification-event (vr-event)
  ((user-value :initarg :user-value :accessor user-value)
   (notification-id :initarg :notification-id :accessor notification-id)))

(defun make-notification-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass overlay-event (vr-event)
  ((overlay-handle :initarg :overlay-handle :accessor overlay-handle)
   (device-path :initarg :device-path :accessor device-path)))

(defun make-overlay-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass status-event (vr-event)
  ((status-state :initarg :status-state :accessor status-state)))

(defun make-status-event (event-type tracked-device-index data-pointer event-age-seconds)
  (make-instance 'status-event :status-state
                 (cffi:foreign-enum-keyword 'vr-state (cffi:mem-ref data-pointer :uint32))
                 ;; according to openvr_driver.h this uint32 is actually an evrstate
                               :event-age event-age-seconds
                               :tracked-device-index tracked-device-index
                               :event-type event-type))

(defclass keyboard-event (vr-event)
  ((new-input :initarg :new-input :accessor new-input)
   (user-value :initarg :user-value :accessor user-value)))

(defun make-keyboard-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass ipd-event (vr-event)
  ((ipd-meters :initarg :ipd-meters :accessor ipd-meters)))

(defun make-ipd-event (event-type tracked-device-index data-pointer event-age-seconds)
  (make-instance 'ipd-event :ipd-meters (cffi:mem-ref data-pointer :float)
                            :event-age event-age-seconds :tracked-device-index tracked-device-index
                            :event-type event-type))

(defclass chaperone-event (vr-event)
  ((previous-universe :initarg :previous-universe :accessor previous-universe)
   (current-universe :initarg :current-universe :accessor current-universe)))

(defun make-chaperone-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass performance-test-event (vr-event)
  ((fidelity-level :initarg :fidelity-level :accessor fidelity-level)))

(defun make-performance-test-event (event-type tracked-device-index data-pointer event-age-seconds)
  (make-instance 'ipd-event :fidelity-level (cffi:mem-ref data-pointer :uint32)
                            :event-age event-age-seconds :tracked-device-index tracked-device-index
                            :event-type event-type))

(defclass touchpad-move-event (vr-event)
  ((finger-down-p :initarg :finger-down-p :accessor finger-down-p)
   (seconds-finger-down :initarg :seconds-finger-down :accessor seconds-finger-down)
   (first-x :initarg :first-x :accessor first-x)
   (first-y :initarg :first-y :accessor first-y)
   (raw-x :initarg :raw-x :accessor raw-x)
   (raw-y :initarg :raw-y :accessor raw-y)))

(defun make-touchPadMove-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass seated-zero-pose-reset-event (vr-event)
  ((reset-by-system-menu-p :initarg :reset-by-system-menu-p :accessor reset-by-system-menu-p)))

(defun make-seatedZeroPoseReset-event
    (event-type tracked-device-index data-pointer event-age-seconds)
  (make-instance 'seated-zero-pose-reset-event
                 :reset-by-system-menu-p (cffi:mem-ref data-pointer :bool)
                 :event-age event-age-seconds
                 :tracked-device-index tracked-device-index
                 :event-type event-type))

(defclass screenshot-event (vr-event)
  ((handle :initarg :handle :accessor handle)
   (screenshot-type :initarg :type :accessor screenshot-type)))

(defun make-screenshot-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass screenshot-progress-event (vr-event)
  ((progress :initarg :progress :accessor progress)))

(defun make-screenshotProgress-event (event-type tracked-device-index data-pointer
                                      event-age-seconds)
  (make-instance 'screenshot-progress-event :progress (cffi:mem-ref data-pointer :float)
                                            :event-age event-age-seconds
                                            :tracked-device-index tracked-device-index
                                            :event-type event-type))

(defclass application-launch-event (vr-event)
  ((pid :initarg :pid :accessor pid)
   (handle-args :initarg :handle-args :accessor handle-args)))

(defun make-applicationLaunch-event
    (event-type tracked-device-index data-pointer event-age-seconds))

(defclass camera-surface-event (vr-event)
  ((overlay-handle :initarg :overlay-handle :accessor overlay-handle)
   (visual-mode :initarg :visual-mode :accessor visual-mode)))

(defun make-cameraSurface-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass message-overlay-event (vr-event)
  ((message-overlay-response :initarg :message-overlay-response
                             :accessor message-overlay-response)))

(defun make-messageOverlay-event (event-type tracked-device-index data-pointer event-age-seconds)
  (make-instance 'message-overlay-event :message-overlay-response
                 (cffi:mem-ref data-pointer 'message-overlay-response)
                                        :event-age event-age-seconds
                                        :tracked-device-index tracked-device-index
                                        :event-type event-type))

(defclass property-event (vr-event)
  ((container :initarg :container :accessor container)
   (property :initarg :property :accessor property)))

(defun make-property-event (event-type tracked-device-index data-pointer event-age-seconds)
  (make-instance 'property-event :event-type event-type
                                 :tracked-device-index tracked-device-index
                                 :event-age event-age-seconds
                                 :property (cffi:mem-ref data-pointer 'tracked-device-property 8)
                                 :container (cffi:mem-ref data-pointer :uint64)))

(defclass haptic-vibration-event (vr-event)
  ((container-handle :initarg :container-handle :accessor container-handle)
   (component-handle :initarg :component-handle :accessor component-handle)
   (duration :initarg :duration :accessor duration)
   (frequency :initarg :frequency :accessor frequency)
   (amplitude :initarg :amplitude :accessor amplitude)))

(defun make-hapticVibration-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass web-console-event (vr-event)
  ((web-console-handle :initarg :web-console-handle :accessor web-console-handle)))

(defun make-webConsole-event (event-type tracked-device-index data-pointer event-age-seconds)
  (make-instance 'web-console-event :web-console-handle (cffi:mem-ref data-pointer :uint64)
                                    :event-age event-age-seconds
                                    :tracked-device-index tracked-device-index
                                    :event-type event-type))

(defclass input-binding-load-event (vr-event)
  ((app-container :initarg :app-container :accessor app-container)
   (message :initarg :message :accessor message)
   (url :initarg :url :accessor url)
   (controller-type :initarg :controller-type :accessor controller-type)))

(defun make-inputBinding-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass action-manifest-event (vr-event)
  ((app-key :initarg :app-key :accessor app-key)
   (message :initarg :message :accessor message)
   (message-param :initarg :message-param :accessor message-param)
   (manifest-path :initarg :manifest-path :accessor manifest-path)))

(defun make-actionManifest-event (event-type tracked-device-index data-pointer event-age-seconds))

(defclass spatial-anchor-event (vr-event)
  ((handle :initarg :handle :accessor handle)))

(defun make-spatialAnchor-event (event-type tracked-device-index data-pointer event-age-seconds)
  (make-instance 'spatial-anchor-event :handle (cffi:mem-ref data-pointer :uint32)
                                       :event-age event-age-seconds
                                       :tracked-device-index tracked-device-index
                                       :event-type event-type))

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
                                                     'event-age-seconds)))
    (if union
        (funcall union
                 type
                 tracked-device-index
                 (cffi:foreign-slot-pointer value '(:struct vr-event-t) 'data)
                 event-age-seconds)
        (list type tracked-device-index event-age-seconds))))

(defparameter *event-type-map* ;; indexed by event-type
  ;; todo: verify + figure out more of these
  (alexandria:plist-hash-table
   (list :none (constantly nil)
         :tracked-device-activated #'make-bare-event
         :tracked-device-deactivated #'make-bare-event
         :tracked-device-updated #'make-bare-event
         :tracked-device-user-interaction-started nil
         :tracked-device-user-interaction-ended nil
         :ipd-changed #'make-ipd-event
         :enter-standby-mode nil
         :leave-standby-mode nil
         :tracked-device-role-changed nil
         :watchdog-wake-up-requested nil
         :lens-distortion-changed nil
         :property-changed #'make-property-event
         :wireless-disconnect nil
         :wireless-reconnect nil
         :button-press #'make-controller-event
         :button-unpress #'make-controller-event
         :button-touch #'make-controller-event
         :button-untouch #'make-controller-event
         :mouse-move #'make-mouse-event
         :mouse-button-down #'make-mouse-event
         :mouse-button-up #'make-mouse-event
         :focus-enter #'make-overlay-event
         :focus-leave #'make-overlay-event
         :scroll #'make-mouse-event
         :touch-pad-move #'make-mouse-event
         :overlay-focus-changed #'make-process-event
         :input-focus-captured #'make-process-event
         :input-focus-released #'make-process-event
         :scene-focus-lost #'make-process-event
         :scene-focus-gained #'make-process-event
         :scene-application-changed #'make-process-event
         :scene-focus-changed #'make-process-event
         :input-focus-changed #'make-process-event
         :scene-application-secondary-rendering-started #'make-process-event
         :hide-render-models nil
         :show-render-models nil
         :overlay-shown nil
         :overlay-hidden nil
         :dashboard-activated nil
         :dashboard-deactivated nil
         :dashboard-thumb-selected #'make-overlay-event
         :dashboard-requested #'make-overlay-event
         :reset-dashboard nil
         :render-toast #'make-notification-event ;;?
         :image-loaded nil
         :show-keyboard nil
         :hide-keyboard nil
         :overlay-gamepad-focus-gained nil
         :overlay-gamepad-focus-lost nil
         :overlay-shared-texture-changed nil
         :dashboard-guide-button-down nil
         :dashboard-guide-button-up nil
         :screenshot-triggered #'make-screenshot-event
         :image-failed nil
         :dashboard-overlay-created nil
         :request-screenshot nil
         :screenshot-taken #'make-screenshot-event
         :screenshot-failed #'make-screenshot-event
         :submit-screenshot-to-dashboard nil
         :screenshot-progress-to-dashboard nil
         :primary-dashboard-device-changed nil
         :notification-shown #'make-notification-event
         :notification-hidden #'make-notification-event
         :notification-begin-interaction #'make-notification-event
         :notification-destroyed #'make-notification-event
         :quit #'make-process-event
         :process-quit #'make-process-event
         :quit-aborted-user-prompt #'make-process-event
         :quit-acknowledged #'make-process-event
         :driver-requested-quit nil
         :chaperone-data-has-changed #'make-chaperone-event
         :chaperone-universe-has-changed #'make-chaperone-event
         :chaperone-temp-data-has-changed #'make-chaperone-event
         :chaperone-settings-have-changed #'make-chaperone-event
         :seated-zero-pose-reset nil
         :audio-settings-have-changed nil
         :background-setting-has-changed nil
         :camera-settings-have-changed nil
         :reprojection-setting-has-changed nil
         :model-skin-settings-have-changed nil
         :environment-settings-have-changed nil
         :power-settings-have-changed nil
         :enable-home-app-settings-have-changed nil
         :status-update #'make-status-event
         :mcimage-updated nil
         :firmware-update-started nil
         :firmware-update-finished nil
         :keyboard-closed #'make-keyboard-event
         :keyboard-char-input #'make-keyboard-event
         :keyboard-done #'make-keyboard-event
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
         :performance-test-enable-capture #'make-performance-test-event
         :performance-test-disable-capture #'make-performance-test-event
         :performance-test-fidelity-level #'make-performance-test-event
         :message-overlay-closed nil
         :message-overlay-close-requested nil
         :vendor-specific-reserved-start nil
         :vendor-specific-reserved-end nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CFFI translators for VREvent_t.

;;; Here we do function-based dispatch to do the type translation as 3b suggested. This is done in
;;; order to avoid cffi's slower runtime dispatch.

(in-package 3b-openvr)

(defclass vr-event ()
  ((event-type :initarg :event-type :accessor event-type)
   (tracked-device-index :initarg :tracked-device-index :accessor tracked-device-index)
   (event-age :initarg :event-age :accessor event-age)
   (tracked-device-pose :accessor tracked-device-pose :initarg :tracked-device-pose)))

(defun make-bare-event (event-type tracked-device-index data-pointer event-age-seconds)
  (declare (ignore data-pointer))
  (make-instance 'vr-event :event-type event-type :tracked-device-index tracked-device-index
                           :event-age event-age-seconds))

(defclass reserved-event (vr-event)
  ((reserved-data-array :initarg :reserved-data-array :accessor reserved-data-array)))

(defun make-reserved-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((reserved-0 reserved-1 reserved-2 reserved-3 reserved-4 reserved-5) data-pointer
                            'vr-event-reserved-t)
    (make-instance 'reserved-event :event-type event-type :tracked-device-index tracked-device-index
                                   :event-age event-age-seconds
                                   :reserved-data-array (vector reserved-0 reserved-1 reserved-2 reserved-3 reserved-4 reserved-5))))

(defclass controller-event (vr-event)
  ((button-id :initarg :button-id :accessor button-id)))

(defun make-controller-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((button) data-pointer 'vr-event-controller-t)
    (make-instance 'controller-event :button-id button
                                     :event-age event-age-seconds
                                     :tracked-device-index tracked-device-index
                                     :event-type event-type)))

(defclass mouse-event (vr-event)
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (button-id :initarg :button-id :accessor button-id)))

(defun make-mouse-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((x y button) data-pointer 'vr-event-mouse-t)
    (make-instance 'mouse-event
                   :x x
                   :y y
                   :button-id button
                   :event-age event-age-seconds
                   :tracked-device-index tracked-device-index
                   :event-type event-type)))

(defclass scroll-event (vr-event)
  ((x-delta :initarg :x-delta :accessor x-delta)
   (y-delta :initarg :y-delta :accessor y-delta)
   (unused :initarg :unused :accessor unused)
   (viewport-scale :initarg :viewport-scale :accessor viewport-scale)))

(defun make-scroll-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((xdelta ydelta unused viewportscale) data-pointer 'vr-event-scroll-t)
    (make-instance 'scroll-event
                   :x-delta xdelta
                   :y-delta ydelta
                   :unused unused
                   :viewport-scale viewportscale
                   :event-age event-age-seconds
                   :tracked-device-index tracked-device-index
                   :event-type event-type)))

(defclass process-event (vr-event)
  ((pid :initarg :x-delta :accessor pid)
   (old-pid :initarg :y-delta :accessor old-pid)
   (forced-p :initarg :forced-p :accessor forced-p)
   (connection-lost-p :initarg :connection-lost-p :accessor connection-lost-p)))

(defun make-process-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((pid old-pid forced connection-lost) data-pointer 'vr-event-process-t)
    (make-instance 'process-event
                   :pid pid
                   :old-pid old-pid
                   :forced-p forced-p
                   :connection-lost-p connection-lost-p
                   :event-age event-age-seconds
                   :tracked-device-index tracked-device-index
                   :event-type event-type)))

(defclass notification-event (vr-event)
  ((user-value :initarg :user-value :accessor user-value)
   (notification-id :initarg :notification-id :accessor notification-id)))

(defun make-notification-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((user-value notification-id) data-pointer 'vr-event-notification-t)
    (make-instance 'notification-event
                   :user-value user-value
                   :notification-id notification-id
                   :event-age event-age-seconds
                   :tracked-device-index tracked-device-index
                   :event-type event-type)))

(defclass overlay-event (vr-event)
  ((overlay-handle :initarg :overlay-handle :accessor overlay-handle)
   (device-path :initarg :device-path :accessor device-path)))

(defun make-overlay-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((overlay-handle device-path) data-pointer 'vr-event-overlay-t)
    (make-instance 'notification-event
                   :overlay-handle overlay-handle
                   :device-path device-path
                   :event-age event-age-seconds
                   :tracked-device-index tracked-device-index
                   :event-type event-type)))

(defclass status-event (vr-event)
  ((status-state :initarg :status-state :accessor status-state)))

(defun make-status-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((status-state) data-pointer vr-event-status-t)
    (make-instance 'status-event :status-state
                   (cffi:foreign-enum-keyword 'vr-state status-state)
                   ;; according to openvr_driver.h this uint32 is actually an evrstate
                   :event-age event-age-seconds
                   :tracked-device-index tracked-device-index
                   :event-type event-type)))

(defclass keyboard-event (vr-event)
  ((new-input :initarg :new-input :accessor new-input)
   (user-value :initarg :user-value :accessor user-value)))

(defun make-keyboard-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((new-input user-value) data-pointer vr-event-keyboard-t)
    (make-instance 'keyboard-event
                   :new-input new-input
                   :user-value user-value
                   :event-age event-age-seconds
                   :tracked-device-index tracked-device-index
                   :event-type event-type)))

(defclass ipd-event (vr-event)
  ((ipd-meters :initarg :ipd-meters :accessor ipd-meters)))

(defun make-ipd-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((ipd-meters) data-pointer vr-event-ipd-t)
    (make-instance 'ipd-event :ipd-meters ipd-meters
                              :event-age event-age-seconds :tracked-device-index tracked-device-index
                              :event-type event-type)))

(defclass chaperone-event (vr-event)
  ((previous-universe :initarg :previous-universe :accessor previous-universe)
   (current-universe :initarg :current-universe :accessor current-universe)))

(defun make-chaperone-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((previous-universe current-universe) data-pointer vr-event-chaperone-t)
    (make-instance 'chaperone-event :previous-universe previous-universe
                                    :current-universe current-universe
                                    :event-age event-age-seconds
                                    :tracked-device-index tracked-device-index
                                    :event-type event-type)))

(defclass performance-test-event (vr-event)
  ((fidelity-level :initarg :fidelity-level :accessor fidelity-level)))

(defun make-performance-test-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((fidelity-level) data-pointer vr-event-performance-test-t)
    (make-instance 'performance-test-event :fidelity-level fidelity-level
                                           :event-age event-age-seconds :tracked-device-index tracked-device-index
                                           :event-type event-type)))

(defclass touchpad-move-event (vr-event)
  ((finger-down-p :initarg :finger-down-p :accessor finger-down-p)
   (seconds-finger-down :initarg :seconds-finger-down :accessor seconds-finger-down)
   (first-x :initarg :first-x :accessor first-x)
   (first-y :initarg :first-y :accessor first-y)
   (raw-x :initarg :raw-x :accessor raw-x)
   (raw-y :initarg :raw-y :accessor raw-y)))

(defun make-touchpad-move-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((finger-down seconds-finger-down value-xfirst value-yfirst value-xraw value-yraw)
                            data-pointer vr-event-touch-pad-move-t)
    (make-instance 'touchpad-move-event :finger-down-p finger-down
                                        :seconds-finger-down seconds-finger-down
                                        :first-x value-xfirst
                                        :first-y value-yfirst
                                        :raw-x value-xraw
                                        :raw-y value-yraw
                                        :event-age event-age-seconds :tracked-device-index tracked-device-index
                                        :event-type event-type)))

(defclass seated-zero-pose-reset-event (vr-event)
  ((reset-by-system-menu-p :initarg :reset-by-system-menu-p :accessor reset-by-system-menu-p)))

(defun make-seated-zero-pose-reset-event
    (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((reset-by-system-menu) data-pointer vr-event-seated-zero-pose-reset-t)
    (make-instance 'seated-zero-pose-reset-event
                   :reset-by-system-menu-p reset-by-system-menu
                   :event-age event-age-seconds
                   :tracked-device-index tracked-device-index
                   :event-type event-type)))

(defclass screenshot-event (vr-event)
  ((handle :initarg :handle :accessor handle)
   (screenshot-type :initarg :screenshot-type :accessor screenshot-type)))

(defun make-screenshot-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((handle type) data-pointer vr-event-screenshot-t)
    (make-instance 'screenshot-event :handle handle
                                     :screenshot-type type
                                     :event-age event-age-seconds
                                     :tracked-device-index tracked-device-index
                                     :event-type event-type)))

(defclass screenshot-progress-event (vr-event)
  ((progress :initarg :progress :accessor progress)))

(defun make-screenshot-progress-event (event-type tracked-device-index data-pointer
                                       event-age-seconds)
  (cffi:with-foreign-slots ((progress) data-pointer vr-event-screenshot-progress-t)
    (make-instance 'screenshot-progress-event :progress progress
                                              :event-age event-age-seconds
                                              :tracked-device-index tracked-device-index
                                              :event-type event-type)))

(defclass application-launch-event (vr-event)
  ((pid :initarg :pid :accessor pid)
   (handle-args :initarg :handle-args :accessor handle-args)))

(defun make-application-launch-event
    (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((pid args-handle) data-pointer vr-event-application-launch-t)
    (make-instance 'application-launch-event :pid pid
                                             :handle-args args-handle
                                             :event-age event-age-seconds
                                             :tracked-device-index tracked-device-index
                                             :event-type event-type)))

(defclass camera-surface-event (vr-event)
  ((overlay-handle :initarg :overlay-handle :accessor overlay-handle)
   (visual-mode :initarg :visual-mode :accessor visual-mode)))

(defun make-camera-surface-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((overlay-handle visual-mode) data-pointer vr-event-editing-camera-surface-t)
    (make-instance 'camera-surface-event :overlay-handle overlay-handle
                                         :visual-mode visual-mode
                                         :event-age event-age-seconds
                                         :tracked-device-index tracked-device-index
                                         :event-type event-type)))

(defclass message-overlay-event (vr-event)
  ((message-overlay-response :initarg :message-overlay-response
                             :accessor message-overlay-response)))

(defun make-message-overlay-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((vr-message-overlay-response) data-pointer vr-event-message-overlay-t)
    (make-instance 'message-overlay-event :message-overlay-response vr-message-overlay-response
                                          :event-age event-age-seconds
                                          :tracked-device-index tracked-device-index
                                          :event-type event-type)))

(defclass property-event (vr-event)
  ((container :initarg :container :accessor container)
   (property :initarg :property :accessor property)))

(defun make-property-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((container prop) data-pointer vr-event-property-t)
    (make-instance 'property-event :event-type event-type
                                   :tracked-device-index tracked-device-index
                                   :event-age event-age-seconds
                                   :property prop
                                   :container container)))

(defclass haptic-vibration-event (vr-event)
  ((container-handle :initarg :container-handle :accessor container-handle)
   (component-handle :initarg :component-handle :accessor component-handle)
   (duration :initarg :duration :accessor duration)
   (frequency :initarg :frequency :accessor frequency)
   (amplitude :initarg :amplitude :accessor amplitude)))

(defun make-haptic-vibration-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((container-handle component-handle duration-seconds frequency amplitude)
                            data-pointer vr-event-haptic-vibration-t) 
    (make-instance 'haptic-vibration-event :container-handle container-handle
                                          :component-handle component-handle
                                          :duration duration-seconds
                                          :frequency frequency
                                          :ämplitude amplitude
                                          :event-type event-type
                                          :tracked-device-index tracked-device-index
                                          :event-age event-age-seconds)))

(defclass web-console-event (vr-event)
  ((web-console-handle :initarg :web-console-handle :accessor web-console-handle)))

(defun make-web-console-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((web-console-handle) data-pointer vr-event-web-console-t) 
    (make-instance 'web-console-event
                   :web-console-handle web-console-handle
                   :event-type event-type
                   :tracked-device-index tracked-device-index
                   :event-age event-age-seconds)))

(defclass input-binding-load-event (vr-event)
  ((app-container :initarg :app-container :accessor app-container)
   (message :initarg :message :accessor message)
   (url :initarg :url :accessor url)
   (controller-type :initarg :controller-type :accessor controller-type)))

(defun make-input-binding-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((app-container path-message path-url path-controller-type)
                            data-pointer vr-event-input-binding-load-t) 
    (make-instance 'input-binding-load-event
                   :app-container app-container
                   :message path-message
                   :url path-url
                   :controller-type path-controller-type
                   :event-type event-type
                   :tracked-device-index tracked-device-index
                   :event-age event-age-seconds)))

(defclass action-manifest-event (vr-event)
  ((app-key :initarg :app-key :accessor app-key)
   (message :initarg :message :accessor message)
   (message-param :initarg :message-param :accessor message-param)
   (manifest-path :initarg :manifest-path :accessor manifest-path)))

(defun make-action-manifest-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((path-app-key path-message path-message-param path-manifest-path)
                            data-pointer vr-event-input-action-manifest-load-t) 
    (make-instance 'action-manifest-event :app-key path-app-key
                                          :message path-message
                                          :message-param path-message-param
                                          :manifest-path path-manifest-path
                                          :event-type event-type
                                          :tracked-device-index tracked-device-index
                                          :event-age event-age-seconds)))

(defclass spatial-anchor-event (vr-event)
  ((handle :initarg :handle :accessor handle)))

(defun make-spatial-anchor-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((handle) data-pointer vr-event-spatial-anchor-t) 
    (make-instance 'spatial-anchor-event :handle handle
                                         :event-type event-type
                                         :tracked-device-index tracked-device-index
                                         :event-age event-age-seconds)))

(defclass progress-update-event (vr-event)
  ((application-property-container :initarg :application-property-container
                                   :accessor application-property-container) 
   (device :initarg :device :accessor device) 
   (source :initarg :source :accessor source) 
   (progress-action :initarg :progress-action :accessor progress-action) 
   (icon :initarg :icon :accessor icon) 
   (progress :initarg :progress :accessor progress)))

(defun make-progress-update-event
    (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots
      ((application-property-container path-device path-input-source path-progress-action path-icon progress)
       data-pointer
       vr-event-progress-update-t) 
    (make-instance 'progress-update-event :application-property-container application-property-container
                                          :device path-device
                                          :source path-input-source
                                          :progress-action path-progress-action
                                          :icon path-icon
                                          :progress progress
                                          :event-type event-type
                                          :tracked-device-index tracked-device-index
                                          :event-age event-age-seconds)))

(defclass show-ui-event (vr-event)
  ((show-ui-type :initarg :show-ui-type :accessor show-ui-type)))

(defun make-show-ui-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((type) data-pointer vr-event-show-ui-t) 
    (make-instance 'show-ui-event :show-ui-type type
                                  :event-type event-type
                                  :tracked-device-index tracked-device-index
                                  :event-age event-age-seconds)))

(defclass show-dev-tools-event (vr-event)
  ((browser-identifier :initarg :browser-identifier :accessor browser-identifier)))

(defun make-dev-tools-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((browser-identifier) data-pointer vr-event-show-dev-tools-t) 
    (make-instance 'show-dev-tools-event :browser-identifier browser-identifier
                                         :event-type event-type
                                         :tracked-device-index tracked-device-index
                                         :event-age event-age-seconds)))

(defclass hdcp-error-event (vr-event)
  ((code :initarg :code :accessor code)))

(defun make-hdcp-error-event (event-type tracked-device-index data-pointer event-age-seconds)
  (cffi:with-foreign-slots ((code) data-pointer vr-event-hdcperror-t) 
    (make-instance 'hdcp-error-event :code code
                                     :event-type event-type
                                     :tracked-device-index tracked-device-index
                                     :event-age event-age-seconds)))

(defmethod cffi:translate-from-foreign (value (type vr-event-t-tclass))
  (let* ((type (cffi:foreign-enum-keyword
                'vr-event-type
                (cffi:foreign-slot-value value '(:struct vr-event-t)
                                         'event-type)))
         (union (gethash type *event-type-map* #'make-bare-event))
         (tracked-device-index (cffi:foreign-slot-value value
                                                        '(:struct vr-event-t)
                                                        'tracked-device-index))
         (event-age-seconds (cffi:foreign-slot-value value
                                                     '(:struct vr-event-t)
                                                     'event-age-seconds)))
    (funcall union ; union should not be nil; if it is then *event-type-map* must be fixed
             type
             tracked-device-index
             (cffi:foreign-slot-pointer value '(:struct vr-event-t) 'data)
             event-age-seconds)))

(defparameter *event-type-map* ;; indexed by event-type
  ;; todo: verify + figure out more of these
  ;; I've tried to guess as many of these as possible.. openvr.h is more helpful than the docs or openvr_capi.h
  ;; A lot of them e.g. watchdog-wake-up-requested are completely incomprehensible,
  ;; and so #'make-bare-event is used for those. Others are informed guesses. This table should be updated as
  ;; appropriate if anyone knows anything new about any event types.
  
  (alexandria:plist-hash-table
   (list :none (constantly nil)
         :tracked-device-activated #'make-bare-event
         :tracked-device-deactivated #'make-bare-event
         :tracked-device-updated #'make-bare-event
         :tracked-device-user-interaction-started #'make-bare-event
         :tracked-device-user-interaction-ended #'make-bare-event
         :ipd-changed #'make-ipd-event
         :enter-standby-mode #'make-bare-event
         :leave-standby-mode #'make-bare-event
         :tracked-device-role-changed #'make-bare-event
         :watchdog-wake-up-requested #'make-bare-event
         :lens-distortion-changed #'make-bare-event
         :property-changed #'make-property-event
         :wireless-disconnect #'make-bare-event
         :wireless-reconnect #'make-bare-event 
         :button-press #'make-controller-event
         :button-unpress #'make-controller-event
         :button-touch #'make-controller-event
         :button-untouch #'make-controller-event
         :modal-cancel  #'make-overlay-event
         :mouse-move #'make-mouse-event
         :mouse-button-down #'make-mouse-event
         :mouse-button-up #'make-mouse-event  
         :focus-enter #'make-overlay-event
         :focus-leave #'make-overlay-event
         :scroll-discrete #'make-scroll-event
         :touch-pad-move #'make-mouse-event
         :overlay-focus-changed #'make-process-event
         :reload-overlays #'make-bare-event
         :scroll-smooth #'make-scroll-event
         :lock-mouse-position #'make-bare-event
         :unlock-mouse-position #'make-bare-event
         :input-focus-captured #'make-process-event
         :input-focus-released #'make-process-event
         :scene-application-changed #'make-process-event
         :scene-focus-changed #'make-process-event
         :input-focus-changed #'make-process-event
         :scene-application-using-wrong-graphics-adapter #'make-process-event
         :action-binding-reloaded #'make-process-event
         :hide-render-models #'make-bare-event
         :show-render-models #'make-bare-event
         :scene-application-state-changed #'make-bare-event
         :console-opened #'make-bare-event
         :console-closed #'make-bare-event
         :overlay-shown #'make-bare-event
         :overlay-hidden #'make-bare-event
         :dashboard-activated #'make-bare-event
         :dashboard-deactivated #'make-bare-event
         :dashboard-requested #'make-overlay-event
         :reset-dashboard #'make-bare-event
         :render-toast #'make-notification-event
         :image-loaded #'make-overlay-event
         :show-keyboard #'make-bare-event
         :hide-keyboard #'make-bare-event
         :overlay-gamepad-focus-gained #'make-overlay-event
         :overlay-gamepad-focus-lost #'make-overlay-event
         :overlay-shared-texture-changed #'make-overlay-event
         :screenshot-triggered #'make-screenshot-event
         :image-failed #'make-overlay-event
         :dashboard-overlay-created #'make-bare-event
         :switch-gamepad-focus #'make-bare-event
         :request-screenshot #'make-bare-event
         :screenshot-taken #'make-screenshot-event
         :screenshot-failed #'make-screenshot-event
         :submit-screenshot-to-dashboard #'make-screenshot-event
         :screenshot-progress-to-dashboard #'make-screenshot-progress-event
         :primary-dashboard-device-changed #'make-bare-event
         :room-view-shown #'make-bare-event
         :room-view-hidden #'make-bare-event
         :show-ui #'make-show-ui-event
         :show-dev-tools #'make-dev-tools-event
         :notification-shown #'make-notification-event
         :notification-hidden #'make-notification-event
         :notification-begin-interaction #'make-notification-event
         :notification-destroyed #'make-notification-event
         :quit #'make-process-event
         :process-quit #'make-process-event
         :quit-acknowledged #'make-process-event
         :driver-requested-quit #'make-bare-event
         :restart-requested #'make-bare-event
         :chaperone-data-has-changed #'make-chaperone-event
         :chaperone-universe-has-changed #'make-chaperone-event
         :chaperone-temp-data-has-changed #'make-chaperone-event
         :chaperone-settings-have-changed #'make-chaperone-event
         :seated-zero-pose-reset #'make-seated-zero-pose-reset-event
         :chaperone-flush-cache #'make-bare-event
         :chaperone-room-setup-starting #'make-bare-event
         :chaperone-room-setup-finishing #'make-bare-event
         :audio-settings-have-changed #'make-bare-event
         :background-setting-has-changed #'make-bare-event
         :camera-settings-have-changed #'make-bare-event
         :reprojection-setting-has-changed #'make-bare-event
         :model-skin-settings-have-changed #'make-bare-event
         :environment-settings-have-changed #'make-bare-event
         :power-settings-have-changed #'make-bare-event
         :enable-home-app-settings-have-changed #'make-bare-event
         :steam-vr-section-setting-changed #'make-bare-event
         :lighthouse-section-setting-changed #'make-bare-event
         :null-section-setting-changed #'make-bare-event
         :user-interface-section-setting-changed #'make-bare-event
         :notifications-section-setting-changed #'make-bare-event
         :keyboard-section-setting-changed #'make-bare-event
         :perf-section-setting-changed #'make-bare-event
         :dashboard-section-setting-changed #'make-bare-event
         :web-interface-section-setting-changed #'make-bare-event
         :trackers-section-setting-changed #'make-bare-event
         :last-known-section-setting-changed #'make-bare-event
         :dismissed-warnings-section-setting-changed #'make-bare-event
         :gpu-speed-section-setting-changed #'make-bare-event
         :status-update #'make-status-event
         :web-interface-install-driver-completed #'make-bare-event
         :mcimage-updated #'make-bare-event
         :firmware-update-started #'make-bare-event
         :firmware-update-finished #'make-bare-event
         :keyboard-closed #'make-keyboard-event
         :keyboard-char-input #'make-keyboard-event
         :keyboard-done #'make-bare-event
         :application-list-updated #'make-application-launch-event
         :application-mime-type-load #'make-application-launch-event
         :process-connected #'make-process-event
         :process-disconnected #'make-process-event
         :compositor-chaperone-bounds-shown #'make-chaperone-event
         :compositor-chaperone-bounds-hidden #'make-chaperone-event
         :compositor-display-disconnected #'make-bare-event
         :compositor-display-reconnected #'make-bare-event
         :compositor-hdcperror #'make-hdcp-error-event
         :compositor-application-not-responding #'make-bare-event
         :compositor-application-resumed #'make-bare-event
         :compositor-out-of-video-memory #'make-bare-event
         :compositor-display-mode-not-supported #'make-bare-event
         :compositor-stage-override-ready #'make-bare-event
         :tracked-camera-start-video-stream #'make-bare-event
         :tracked-camera-stop-video-stream #'make-bare-event
         :tracked-camera-pause-video-stream #'make-bare-event
         :tracked-camera-resume-video-stream #'make-bare-event
         :tracked-camera-editing-surface #'make-camera-surface-event
         :performance-test-enable-capture #'make-performance-test-event
         :performance-test-disable-capture #'make-performance-test-event
         :performance-test-fidelity-level #'make-performance-test-event
         :message-overlay-closed #'make-message-overlay-event
         :message-overlay-close-requested #'make-message-overlay-event
         :input-haptic-vibration #'make-haptic-vibration-event
         :input-binding-load-failed #'make-input-binding-event
         :input-binding-load-successful #'make-input-binding-event
         :input-action-manifest-reloaded #'make-bare-event
         :input-action-manifest-load-failed #'make-action-manifest-event
         :input-progress-update #'make-progress-update-event
         :input-tracker-activated #'make-bare-event
         :input-bindings-updated #'make-input-binding-event
         :input-binding-subscription-changed #'make-input-binding-event
         :spatial-anchors-pose-updated #'make-spatial-anchor-event
         :spatial-anchors-descriptor-updated #'make-spatial-anchor-event
         :spatial-anchors-request-pose-update #'make-spatial-anchor-event 
         :spatial-anchors-request-descriptor-update #'make-spatial-anchor-event 
         :system-report-started #'make-bare-event
         :monitor-show-headset-view #'make-process-event
         :monitor-hide-headset-view #'make-process-event
         :vendor-specific-reserved-start #'make-reserved-event
         :vendor-specific-reserved-end #'make-reserved-event)))

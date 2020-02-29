;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level API for the IVRSystem API

;;; version = IVR_System_021

(in-package 3b-openvr)

(annot:enable-annot-syntax)

@export
(defun get-recommended-render-target-size (&key (system *system*))
  "Suggested size for the intermediate render target that the distortion pulls from."
  (cffi:with-foreign-objects ((w :uint32)
                              (h :uint32))
    (%get-recommended-render-target-size (table system) w h)
    ;; should this return list, values, or some struct/class or something?
    (values (cffi:mem-ref w :uint32) (cffi:mem-ref h :uint32))))


@export
(defun get-projection-matrix (eye near far &key (system *system*))
  "The projection matrix for the specified eye."
  (%get-projection-matrix (table system) eye near far))

@export
(defun projection-raw (eye &key (system *system*))
  "The components necessary to build your own projection matrix in case your application is doing something 
   fancy like infinite Z."
  (cffi:with-foreign-objects ((left :float)
                              (right :float)
                              (top :float)
                              (bottom :float))
    (%get-projection-raw (table system) eye left right top bottom)
    (values (cffi:mem-ref left :float) (cffi:mem-ref right :float)
            (cffi:mem-ref top :float) (cffi:mem-ref bottom :float))))

@export
(defun compute-distortion (eye u v &key (system *system*))
  "Gets the result of the distortion function for the specified eye and input UVs. UVs go from 0,0 in 
   the upper left of that eye's viewport and 1,1 in the lower right of that eye's viewport."
  (cffi:with-foreign-object (pointer '(:struct distortion-coordinates-t))
    (if (%compute-distortion (table system) eye y v pointer)
        (values (cffi:foreign-slot-value pointer '(:struct distortion-coordinates-t) 'red)
                (cffi:foreign-slot-value pointer '(:struct distortion-coordinates-t) 'green)
                (cffi:foreign-slot-value pointer '(:struct distortion-coordinates-t) 'blue))
        (error "ComputeDistortion returned false"))))

@export
(defun eye-to-head-transform (eye &key (system *system*))
  "Returns the transform from eye space to the head space. Eye space is the per-eye flavor of head
   space that provides stereo disparity. Instead of Model * View * Projection the sequence is 
   Model * View * Eye^-1 * Projection. 
   Normally View and Eye^-1 will be multiplied together and treated as View in your application. "
  (%get-eye-to-head-transform (table system) eye))

@export
(defun time-since-last-vsync (&key (system *system*))
  (cffi:with-foreign-objects ((seconds :float)
                              (frame-counter :uint64))
    (if (%get-time-since-last-vsync (table system) seconds frame-counter)
        (values t (cffi:mem-ref seconds :float)
                (cffi:mem-ref frame-counter :uint64))
        (values nil 0 0))))

;; (defun d3d9-adapter-index) 
;; (defun dxgi-output-info)
;; (defun output-device) ; vulkan and direct 9

@export
(defun display-on-desktop-p (&key (system *system*))
  "Use to determine if the headset display is part of the desktop (i.e. extended) or hidden (i.e. direct mode)."
  (%is-display-on-desktop (table system)))

@export
(defun set-display-visibility (visibile-on-desktop &key (system *system*))
  "Set the display visibility (true = extended, false = direct mode).
   Return value of T indicates that the change was successful."
  (%set-display-visibility (table system) visibile-on-desktop))

(defun device-to-absolute-tracking-pose ())

@export
(defun reset-seated-zero-pose (&key (system *system*))
  "Sets the zero pose for the seated tracker coordinate system to the current position and yaw of the HMD. After 
   ResetSeatedZeroPose all GetDeviceToAbsoluteTrackingPose calls that pass TrackingUniverseSeated as the origin 
   will be relative to this new zero pose. The new zero coordinate system will not change the fact that the Y axis 
   is up in the real world, so the next pose returned from GetDeviceToAbsoluteTrackingPose after a call to 
   ResetSeatedZeroPose may not be exactly an identity matrix.
   NOTE: This function overrides the user's previously saved seated zero pose and should only be called as the result
   of a user action. 
   Users are also able to set their seated zero pose via the OpenVR Dashboard."
  (%reset-seated-zero-pose (table system)))

@export
(defun seated-zero-pose-to-standing-absolute-tracking-pose (&key (system *system*))
  "Returns the transform from the seated zero pose to the standing absolute tracking system. This allows 
   applications to represent the seated origin to used or transform object positions from one coordinate
   system to the other. 
   The seated origin may or may not be inside the Play Area or Collision Bounds returned by IVRChaperone. Its position 
   depends on what the user has set from the Dashboard settings and previous calls to ResetSeatedZeroPose."
  (%get-seated-zero-pose-to-standing-absolute-tracking-pose (table system)))

@export
(defun raw-zero-pose-to-standing-absolute-tracking-pose (&key (system *system*))
  "Returns the transform from the tracking origin to the standing absolute tracking system. This allows
   applications to convert from raw tracking space to the calibrated standing coordinate system."
  (%get-raw-zero-pose-to-standing-absolute-tracking-pose (table system)))

@export
(defun sorted-tracked-device-indices-of-class ())

@export
(defun tracked-device-activity-level (device-id &key (system *system*))
  "Returns the level of activity on the device."
  (%get-tracked-device-activity-level (table system) device-id))

(defun apply-transform ())

@export
(defun tracked-device-index-for-controller-role (role &key (system *system*))
  "Returns the device index associated with a specific role, for example the left hand or the right hand.
   This function is deprecated in favor of the new IVRInput system."
  (let ((i (%get-tracked-device-index-for-controller-role (table system) role)))
    (if (= i #xffffffff)
        nil
        i)))

@export
(defun controller-role-for-tracked-device-index (index &key (system *system*))
  "Returns the controller type associated with a device index.
   This function is deprecated in favor of the new IVRInput system."
  (%get-controller-role-for-tracked-device-index (table system) index))

;; property methods


@export
(defun tracked-device-class (index &key (system *system*))
  "Returns the device class of a tracked device. If there has not been a device connected in this slot
   since the application started this function will return TrackedDevice_Invalid. For previous detected
   devices the function will return the previously observed device class. 
   To determine which devices exist on the system, just loop from 0 to k_unMaxTrackedDeviceCount and check
   the device class. Every device with something other than TrackedDevice_Invalid is associated with an 
   actual tracked device."
  (%get-tracked-device-class (table system) index))

@export
(defun tracked-device-connected-p (index &key (system *system*))
  "Returns T if there is a device connected in this slot."
  (%is-tracked-device-connected (table system) index))


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

(defun array-tracked-device-property ())

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

@export
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

(defun poll-next-event-with-pose ())

@export
(defun hidden-area-mesh (eye type &key (system *system*))
  "Returns the hidden area mesh for the current HMD. The pixels covered by this mesh will never be seen by the user
   after the lens distortion is applied based on visibility to the panels. If this HMD does not have a hidden area 
   mesh, the vertex data and count will be NULL and 0 respectively. This mesh is meant to be rendered into the stencil
   buffer (or into the depth buffer setting nearz) before rendering each eye's view. 
   This will improve performance by letting the GPU early-reject pixels the user will never see before running the 
   pixel shader.
   NOTE: Render this mesh with backface culling disabled since the winding order of the vertices can be different 
   per-HMD or per-eye.
   Setting the bInverse argument to true will produce the visible area mesh that is commonly used in place of 
   full-screen quads. The visible area mesh covers all of the pixels the hidden area mesh does not cover.
   Setting the bLineLoop argument will return a line loop of vertices in HiddenAreaMesh_t->pVertexData with 
   HiddenAreaMesh_t->unTriangleCount set to the number of vertices."
  (%get-hidden-area-mesh (table system) eye type))

@export
(defun get-controller-state (device &key (system *system*))
  (cffi:with-foreign-object (state '(:struct vr-controller-state-001-t))
    (when (%get-controller-state (table system) device state
                                 (cffi:foreign-type-size
                                  '(:struct vr-controller-state-001-t)))
      (cffi:mem-ref state '(:struct vr-controller-state-001-t)))))

(defun get-controller-state-with-pose ())

@export
(defun trigger-haptic-pulse (device-index axis-id duration-in-microseconds &key (system *system*))
  "Trigger a single haptic pulse on a controller. After this call the application may not trigger another haptic pulse 
   on this controller and axis combination for 5ms. 
   This function is deprecated in favor of the new IVRInput system."
  (%trigger-haptic-pulse (table system) device-index axis-id duration-in-microseconds))

@export
(defun input-available-p (&key (system *system*))
  "Returns true if this application is receiving input from the system. This would return false if system-related 
   functionality is consuming the input stream."
  (%is-input-available (table system)))

@export
(defun steam-vr-drawing-controllers-p (&key (system *system*))
  "Returns true SteamVR is drawing controllers on top of the application. Applications should consider
   not drawing anything attached to the user's hands in this case."
  (%is-steam-vr-drawing-controllers (table system)))

@export
(defun should-application-pause-p (&key (system *system*))
  "Returns true if the user has put SteamVR into a mode that is distracting them from the application.
   For applications where this is appropriate, the application should pause ongoing activity."
  (%should-application-pause (table system)))

@export
(defun should-application-reduce-rendering-work-p (&key (system *system*))
  "Returns true if SteamVR is doing significant rendering work and the game should do what it can to reduce
   its own workload. One common way to do this is to reduce the size of the render target provided for each eye."
  (%should-application-reduce-rendering-work (table system)))

;; firmware methods

@export
(defun perform-firmware-update (tracked-device-index &key (system *system*))
  "Performs the actual firmware update if applicable. 
   The following events will be sent, if VRFirmwareError_None was returned: VREvent_FirmwareUpdateStarted, 
   VREvent_FirmwareUpdateFinished 
   Use the properties Prop_Firmware_UpdateAvailable_Bool, Prop_Firmware_ManualUpdate_Bool, and 
   Prop_Firmware_ManualUpdateURL_String to figure our whether a firmware update is available, and to figure out whether
   its a manual update 
   Prop_Firmware_ManualUpdateURL_String should point to an URL describing the manual update process."
  (let ((error-code (%perform-firmware-update (table system) tracked-device-index)))
    (when (eq error-code :fail)
      (error "Firmware update error"))))

@export
(defun acknowledge-quit-exiting (&key (system *system*))
  "Call this to acknowledge to the system that VREvent_Quit has been received and that the process is exiting.
   This extends the timeout until the process is killed."
  (%acknowledge-quit-exiting (table system)))

@export
(defun app-container-file-paths (&key (system *system*))
  "Retrieves a null-terminated, semicolon-delimited list of UTF8 file paths that an application 
   must have read access to when running inside of an app container. Returns the number of bytes
   needed to hold the list."
  (let ((length (%get-app-container-file-paths (table system) (cffi:null-pointer) 0)))
    (cffi:with-foreign-string (pointer (make-string length))
      (%get-app-container-file-paths (table system) pointer length)
      (cffi:foreign-string-to-lisp pointer))))

@export
(defun runtime-version ()
  "Returns the current version of the SteamVR runtime."
  (cffi:foreign-string-to-lisp (%get-runtime-version (table system))))


;;;;;

@export
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

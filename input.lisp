;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the IVRInput API.

;;; IVRInput_007

(in-package 3b-openvr)

(defun set-action-manifest-path (action-manifest-path &key (input *input*))
  "Sets the path to the action manifest JSON file that is used by this application. If this
   information was set on the Steam partner site, calls to this function are ignored. If the Steam 
   partner site setting and the path provided by this call are different, 
   VRInputError_MismatchedActionManifest is returned. This call must be made before the first call 
   to #'update-action-state or #'poll-next-event."
  (%set-action-manifest-path (table input) action-manifest-path))
;; (set-action-manifest-path "/home/selwyn/openvr/samples/bin/hellovr_actions.json")
(defclass input-action-data ()
  (active-origin active-p))

(defclass input-binding-info ()
  (device-pathname input-pathname mode-name slot-name))

(define-clos-wrapper (input-skeletal-action-data input-skeletal-action-data-t) (input-action-data)
                     ((active-p active) (active-origin active-origin)))

(define-clos-wrapper (input-pose-action-data input-pose-action-data-t) (input-action-data)
                     ((active-p active) (active-origin active-origin) (pose pose)))

(define-clos-wrapper (input-digital-action-data input-digital-action-data-t) (input-action-data)
                     ((active-p active) (active-origin active-origin) (state-p state) (changed-p changed)
                      (update-time update-time)))

(define-clos-wrapper (input-analog-action-data input-analog-action-data-t) (input-action-data)
                     ((x x) (y y)(z z) (delta-x delta-x) (delta-y delta-y) (delta-z delta-z) (update-time update-time)
                      (active-p active) (active-origin active-origin)))

(define-clos-wrapper (input-origin-info input-origin-info-t) ()
                     ((device-path device-path) (tracked-device-index tracked-device-index)
                      (render-model-component-name rch-render-model-component-name)))

(define-clos-wrapper (active-action-set vr-active-action-set-t) ()
                     ((action-set-handle action-set) (restricted-to-device restricted-to-device)
                      (secondary-action-set secondary-action-set) (priority priority) (padding padding)))

(define-clos-wrapper (skeletal-summary-data vr-skeletal-summary-data-t) ()
                     ((finger-curl finger-curl) (finger-splay finger-splay)))

(define-clos-wrapper (bone-transform vr-bone-transform-t) ()
                     ((location position) (orientation orientation)))

;; handle management

(defun action-set (action-set-name &key (input *input*))
  "Returns an integer representing an action set. This object is used for all performance-sensitive calls."
  (cffi:with-foreign-object (handle-pointer 'vr-action-set-handle-t)
    (let ((result (%get-action-set-handle (table input) action-set-name handle-pointer)))
      (unless (eq :none result)
        (error "input error ~a" result)))
    (cffi:mem-ref handle-pointer 'vr-action-set-handle-t)))

(defun action (action-name &key (input *input*))
  "Returns an integer representing an action. This handle is used for all performance-sensitive calls."
  (cffi:with-foreign-object (handle-pointer 'vr-action-handle-t)
    (%get-action-handle (table input) action-name handle-pointer)
    (cffi:mem-ref handle-pointer 'vr-action-handle-t)))

(defun input-source (input-source-pathname &key (input *input*))
  "Returns an integer representing an input source for any path in the input system. E.g. /user/hand/right"
  (cffi:with-foreign-object (handle-pointer 'vr-input-value-handle-t)
    (%get-input-source-handle (table input) (namestring input-source-pathname) handle-pointer)
    (cffi:mem-ref handle-pointer 'vr-input-value-handle-t)))

;; reading action state
(defun update-action-state (active-action-sets &key (input *input*))
  (cffi:with-foreign-object (pointer '(:struct vr-active-action-set-t) (length active-action-sets))
    (loop for i below (length active-action-sets)
          do (setf (cffi:mem-ref pointer '(:struct vr-active-action-set-t) i)
                   (nth i active-action-sets)))
    (%update-action-state (table input) pointer (cffi:foreign-type-size 'vr-active-action-set-t-tclass)
                          (length active-action-sets))))

(defun digital-action-data (action &key (restrict-to-device +invalid-input-value-handle+) (input *input*))
  (%get-digital-action-data (table input)))

(defun analog-action-data (action &key (restrict-to-device +invalid-input-value-handle+) (input *input*))
  (%get-analog-action-data (table input)))

(defun pose-action-data-relative-to-now (action tracking-universe-origin
                                         &key (restrict-to-device +invalid-input-value-handle+) (input *input*))
  (%get-pose-action-data-relative-to-now (table input)))

(defun pose-action-data-for-next-frame (action tracking-universe-origin
                                        &key (restrict-to-device +invalid-input-value-handle+) (input *input*))
  (%get-pose-action-data-for-next-frame (table input)))

(defun skeletal-action-data (action &key (input *input*))
  (%get-skeletal-action-data (table input)))

;; static skeletal data

(defun bone-count (action &key (input *input*))
  "Reads the number of bones in skeleton associated with the given action."
  (cffi:with-foreign-object (pointer :uint32)
    (let ((result (%get-bone-count (table input) action pointer)))
      (unless (eq result :none)
        (error "Input error: ~a" result)))
    (cffi:mem-ref pointer :uint32)))

(defun bone-hierarchy (action &key (input *input*))
  "Returns an array of each bone's parent in the skeleton associated with the given action."
  (error "implement me"))

(defun bone-name (action index &key (input *input*))
  "Returns the name of the bone at the given index in the skeleton associated with the given action."
  (cffi:with-foreign-string (foreign-string (make-string 512))
    (%get-bone-name (table input) action index foreign-string 512)
    (cffi:foreign-string-to-lisp foreign-string)))

(defun skeletal-reference-transforms (action skeletal-transform-space skeletal-reference-pose &key (input *input*))
  "Returns the transforms for a specific static skeletal reference pose."
  (error "implement me"))

(defun skeletal-tracking-level (action &key (input *input*))
  "Reads the level of accuracy to which the controller is able to track the user to recreate a skeletal pose."
  (cffi:with-foreign-object (pointer '(:pointer vr-skeletal-tracking-level))
    (%get-skeletal-tracking-level (table input) action pointer)
    (cffi:mem-ref pointer 'vr-skeletal-tracking-level)))

;; dynamic skeletal data

(defun skeletal-bone-data (action skeletal-transform-space motion-range &key (input *input*))
  "Returns the state of the skeletal bone data associated with this action."
  (error "implement me"))

(defun skeletal-summary-data (action &key (input *input*))
  "Reads summary information about the current pose of the skeleton associated with the given action."
  (cffi:with-foreign-object (pointer '(:pointer (:struct vr-skeletal-summary-data-t)))
    (%get-skeletal-summary-data (table action) action pointer)
    (cffi:mem-ref pointer '(:struct vr-skeletal-summary-data-t))))
 
(defun compressed-skeletal-bone-data (action motion-range &key (input *input*))
  "Reads the state of the skeletal bone data in a compressed form that is suitable for sending over the network. The
   required buffer size will never exceed ( sizeof(VR_BoneTransform_t)*boneCount + 2).
   Usually the size will be much smaller."
  (error "implement me"))

(defun decompress-skeletal-bone-data (buffer skeletal-transform-space &key (input *input*))
  "Turns a compressed buffer from #'compressed-skeletal-bone-data and turns it back into a bone transform array."
  (error "implement me"))


;; haptics
(defun trigger-haptic-vibration-action (action from-now duration frequency amplitude restrict-to-device &key (input *input*))
  "Triggers a haptic event as described by the specified action."
  (%trigger-haptic-vibration-action (table input) action from-now duration frequency amplitude restrict-to-device))

;; action origins

(defun action-origins (action-set action &key (input *input*))
  "Retrieve origins for an action"
  (cffi:with-foreign-object (pointer '(:pointer vr-input-value-handle-t) 10)
    (%get-action-origins (table input) action-set action pointer 10)
    (let ((result (make-array '(10))))
      (loop for i below 10 do (setf (aref result i) (cffi:mem-ref pointer 'vr-input-value-handle-t i))
            finally (return result)))))

(defun origin-localized-name (origin &key (input *input*))
  "application to specify which parts of the origin's information it wants a string for. */"
  (cffi:with-foreign-string (foreign-string (make-string 512 :initial-element #\Space))
    (%get-origin-localized-name (table input) origin foreign-string 512 64)
    (cffi:foreign-string-to-lisp foreign-string)))

(defun origin-tracked-device-information (input-source &key (input *input*))
  "Retrieves useful information for the origin of this action."
  (error "implement me"))

(defun action-binding-information (action)
  "Retrieves useful information about the bindings for an action."
  (error "implement me"))

(defun show-action-origins (action-set action &key (input *input*))
  "Shows the current binding for the action in the headset."
  (%show-action-origins (table input) action-set action))

(defun show-bindings-for-action-set (action-sets origin-to-highlight &key (input *input*))
  (error "implement me"))

;; legacy input

(defun using-legacy-input-p (&key (input *input*))
  (%is-using-legacy-input (table input)))

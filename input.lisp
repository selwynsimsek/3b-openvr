;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the IVRInput API.

;;; IVRInput_007

(in-package 3b-openvr)

(annot:enable-annot-syntax)

(defmacro with-input-error (&rest body)
  (let ((error-name (gensym "error-value")))
    `(let ((,error-name (progn ,@body)))
       (if (eq ,error-name :none) t (error "VR input error: ~a" ,error-name)))))

@export
(defclass input-action-data ()
  (active-origin active-p))

(define-clos-wrapper (input-binding-info input-binding-info-t) ()
                     ((device-path rch-device-path-name)
                      (input-path rch-input-path-name)
                      (mode-name rch-mode-name)
                      (slot-name rch-slot-name)))

(define-clos-wrapper (tracked-device-pose tracked-device-pose-t) ()
                     ((device-to-absolute-tracking device-to-absolute-tracking)
                      (velocity velocity)
                      (angular-velocity angular-velocity)
                      (tracking-result tracking-result)
                      (pose-valid-p pose-is-valid)
                      (device-connected-p device-is-connected)))

(define-clos-wrapper (input-skeletal-action-data input-skeletal-action-data-t) (input-action-data)
                     ((active-p active) (active-origin active-origin)))

(define-clos-wrapper (input-pose-action-data input-pose-action-data-t) (input-action-data)
                     ((active-p active) (active-origin active-origin) (pose pose)))

(define-clos-wrapper (input-digital-action-data input-digital-action-data-t) (input-action-data)
                     ((active-p active) (active-origin active-origin) (state-p state)
                      (changed-p changed) (update-time update-time)))

(define-clos-wrapper (input-analog-action-data input-analog-action-data-t) (input-action-data)
                     ((x x) (y y)(z z) (delta-x delta-x) (delta-y delta-y) (delta-z delta-z)
                      (update-time update-time)
                      (active-p active) (active-origin active-origin)))

(define-clos-wrapper (input-origin-info input-origin-info-t) ()
                     ((device-path device-path) (tracked-device-index tracked-device-index)
                      (render-model-component-name rch-render-model-component-name)))

(define-clos-wrapper (active-action-set vr-active-action-set-t) ()
                     ((action-set-handle action-set) (restricted-to-device restricted-to-device)
                      (secondary-action-set secondary-action-set) (priority priority)
                      (padding padding)))

(define-clos-wrapper (skeletal-summary-data vr-skeletal-summary-data-t) ()
                     ((finger-curl finger-curl) (finger-splay finger-splay)))

(define-clos-wrapper (bone-transform vr-bone-transform-t) ()
                     ((location position) (orientation orientation)))

;; handle management

@export
(defun set-action-manifest-path (action-manifest-path &key (input *input*))
  "Sets the path to the action manifest JSON file that is used by this application. If this
   information was set on the Steam partner site, calls to this function are ignored. If the Steam 
   partner site setting and the path provided by this call are different, 
   VRInputError_MismatchedActionManifest is returned. This call must be made before the first call 
   to #'update-action-state or #'poll-next-event."
  (with-input-error (%set-action-manifest-path (table input) action-manifest-path))) ; think it works
;; (set-action-manifest-path "/home/selwyn/openvr/samples/bin/hellovr_actions.json")


@export
(defun action-set (action-set-name &key (input *input*))
  "Returns an integer representing an action set. This object is used for all performance-sensitive
   calls."
  (cffi:with-foreign-object (handle-pointer 'vr-action-set-handle-t)
    (with-input-error
        (%get-action-set-handle (table input) action-set-name handle-pointer))
    (cffi:mem-ref handle-pointer 'vr-action-set-handle-t))) ; works

@export
(defun action (action-name &key (input *input*))
  "Returns an integer representing an action. This handle is used for all performance-sensitive 
   calls."
  (cffi:with-foreign-object (handle-pointer 'vr-action-handle-t)
    (with-input-error
        (%get-action-handle (table input) action-name handle-pointer))
    (cffi:mem-ref handle-pointer 'vr-action-handle-t))) ; works

@export
(defun input-source (input-source-pathname &key (input *input*))
  "Returns an integer representing an input source for any path in the input system.
   E.g. /user/hand/right"
  (cffi:with-foreign-object (handle-pointer 'vr-input-value-handle-t)
    (with-input-error
        (%get-input-source-handle (table input) (namestring input-source-pathname) handle-pointer))
    (cffi:mem-ref handle-pointer 'vr-input-value-handle-t))) ; works?

;; reading action state

@export
(defun update-action-state (active-action-sets &key (input *input*))
  "Reads the current state into all actions. After this call, the results of *action-data calls 
   will be the same until the next call to #'update-action-state."
  (cffi:with-foreign-object (pointer '(:struct vr-active-action-set-t) (length active-action-sets))
    (loop for i below (length active-action-sets)
          do
          (setf (cffi:mem-aref pointer '(:struct vr-active-action-set-t) i)
                   (aref active-action-sets i)))
    (with-input-error
        (%update-action-state (table input)
                              pointer
                              (cffi:foreign-type-size '(:struct vr-active-action-set-t))
                              (length active-action-sets))))) ; works

@export
(defun digital-action-data (action &key (restrict-to-device +invalid-input-value-handle+)
                                        (input *input*))
  "Reads the state of a digital action."
  (cffi:with-foreign-object (pointer '(:struct input-digital-action-data-t))
    (with-input-error
        (%get-digital-action-data (table input) action pointer
                                  (cffi:foreign-type-size '(:struct input-digital-action-data-t))
                                  restrict-to-device))
    (cffi:mem-ref pointer '(:struct input-digital-action-data-t)))) ; works

@export
(defun analog-action-data (action &key (restrict-to-device +invalid-input-value-handle+)
                                       (input *input*))
  "Reads the state of an analog action given its handle."
  (cffi:with-foreign-object (pointer '(:struct input-analog-action-data-t))
    (with-input-error
        (%get-analog-action-data (table input) action pointer
                                 (cffi:foreign-type-size '(:struct input-analog-action-data-t))
                                 restrict-to-device))
    (cffi:mem-ref pointer '(:struct input-analog-action-data-t))))

@export
(defun pose-action-data-relative-to-now (action tracking-universe-origin predicted-seconds-from-now
                                         &key (restrict-to-device +invalid-input-value-handle+)
                                              (input *input*))
  "Reads the state of a pose action for the number of seconds relative to now. This will generally
   be called with negative times from the update-time fields in other actions."
  (cffi:with-foreign-object (pointer '(:struct input-pose-action-data-t))
    (with-input-error
        (%get-pose-action-data-relative-to-now (table input) action tracking-universe-origin
                                               predicted-seconds-from-now pointer
                                               (cffi:foreign-type-size
                                                '(:struct input-pose-action-data-t))
                                               restrict-to-device))
    (cffi:mem-ref pointer '(:struct input-pose-action-data-t))))

@export
(defun pose-action-data-for-next-frame (action tracking-universe-origin
                                        &key (restrict-to-device +invalid-input-value-handle+)
                                             (input *input*))
  "Reads the state of a pose action. The returned values will match the values returned by the 
   last call to #'wait-get-poses."
  (cffi:with-foreign-object (pointer '(:struct input-pose-action-data-t))
    (with-input-error
        (%get-pose-action-data-for-next-frame
         (table input)
         action
         tracking-universe-origin
         pointer
         (cffi:foreign-type-size '(:struct input-pose-action-data-t))
         restrict-to-device))
    (cffi:mem-ref pointer '(:struct input-pose-action-data-t)))) ; works

@export
(defun skeletal-action-data (action &key (input *input*))
  "Reads the state of a skeletal action given its handle."
  (cffi:with-foreign-object (pointer '(:struct input-skeletal-action-data-t))
    (with-input-error
        (%get-skeletal-action-data (table input) action pointer
                                   (cffi:foreign-type-size
                                    '(:struct input-skeletal-action-data-t))))
    (cffi:mem-ref pointer '(:struct input-skeletal-action-data-t))))

;; static skeletal data

@export
(defun bone-count (action &key (input *input*))
  "Reads the number of bones in skeleton associated with the given action."
  (cffi:with-foreign-object (pointer :uint32)
    (with-input-error (%get-bone-count (table input) action pointer))
    (cffi:mem-ref pointer :uint32))) ; works

@export
(defun bone-hierarchy (action &key (input *input*))
  "Returns an array of each bone's parent in the skeleton associated with the given action."
  #-sbcl (do-bone-hierarchy action input)
  #+sbcl (sb-int:with-float-traps-masked (:invalid)
           (do-bone-hierarchy action input))) ;works

(defun do-bone-hierarchy (action input)
  (let ((count (bone-count action)))
    (cffi:with-foreign-object (pointer 'bone-index-t count)
      (with-input-error (%get-bone-hierarchy (table input) action pointer count))
      (let ((result (make-array (list count))))
        (loop for i from 0 below count
              do (setf (aref result i) (cffi:mem-aref pointer 'bone-index-t i))
              finally (return result)))))) ; works

@export
(defun bone-name (action bone &key (input *input*))
  "Returns the name of the bone in the skeleton associated with the given action."
  (cffi:with-foreign-string (foreign-string (make-string (1- +max-bone-name-length+)))
    #-sbcl
    (with-input-error (%get-bone-name (table input) action bone foreign-string (1- +max-bone-name-length+)))
    #+sbcl
    (sb-int:with-float-traps-masked (:invalid)
      (with-input-error (%get-bone-name (table input) action bone foreign-string (1- +max-bone-name-length+))))
    (cffi:foreign-string-to-lisp foreign-string))) ; works

@export
(defun skeletal-reference-transforms (action skeletal-transform-space skeletal-reference-pose
                                      &key (input *input*))
  "Returns the bone transforms for a specific static skeletal reference pose."
  (let ((count (bone-count action)))
    (cffi:with-foreign-object (pointer '(:struct vr-bone-transform-t) count)
      (with-input-error (%get-skeletal-reference-transforms
                         (table input) action skeletal-form-space
                         skeletal-reference-pose pointer count))
      (let ((result (make-array (list count))))
        (loop for i below count
              do (setf (aref result i) (cffi:mem-ref pointer '(:struct vr-bone-transform-t) i))
              finally (return result))))))

@export
(defun skeletal-tracking-level-for-handle (handle &key (input *input*))
  "Reads the level of accuracy to which the controller is able to track the user to recreate a 
   skeletal pose."
  (cffi:with-foreign-object (pointer '(:pointer vr-skeletal-tracking-level))
    (with-input-error (%get-skeletal-tracking-level (table input) handle pointer))
    (cffi:mem-ref pointer 'vr-skeletal-tracking-level)))

;; dynamic skeletal data

@export
(defun skeletal-bone-data (action skeletal-transform-space motion-range &key (input *input*))
  "Returns the state of the skeletal bone data associated with this action."
  (let ((count (bone-count action)))
    (cffi:with-foreign-object (pointer '(:struct vr-bone-transform-t) count)
      (with-input-error
          (%get-skeletal-bone-data (table input) action skeletal-transform-space
                                   motion-range pointer count))
      (let ((result (make-array (list count))))
        (loop for i below count
              do (setf (aref result i) (cffi:mem-ref pointer '(:struct vr-bone-transform-t) i))
              finally (return result))))))

@export
(defun skeletal-summary-data (action &key (input *input*) (summary-type :animation))
  "Reads summary information about the current pose of the skeleton associated with the given
   action."
  (cffi:with-foreign-object (pointer '(:pointer (:struct vr-skeletal-summary-data-t)))
    (%get-skeletal-summary-data (table input) action summary-type pointer)
    (cffi:mem-ref pointer '(:struct vr-skeletal-summary-data-t))))

(defun compressed-skeletal-bone-data (action motion-range &key (input *input*))
  "Reads the state of the skeletal bone data in a compressed form that is suitable for sending over
   the network.
   The required buffer size will never exceed (sizeof(VR_BoneTransform_t)*boneCount + 2).
   Usually the size will be much smaller."
  (error "implement me")) ; how to do this?

(defun decompress-skeletal-bone-data (buffer skeletal-transform-space &key (input *input*))
  "Turns a compressed buffer from #'compressed-skeletal-bone-data and turns it back into a bone 
   transform array."
  (error "implement me")) ; how to do this?

;; haptics

@export
(defun trigger-haptic-vibration-action (action from-now duration frequency amplitude
                                        restrict-to-device &key (input *input*))
  "Triggers a haptic event as described by the specified action."
  (with-input-error
      (%trigger-haptic-vibration-action (table input) action from-now duration frequency amplitude
                                        restrict-to-device))) ; works

;; action origins

@export
(defun action-origins (action-set action &key (input *input*))
  "Retrieve origins for an action."
  (cffi:with-foreign-object (pointer '(:pointer vr-input-value-handle-t) 10)
    (%get-action-origins (table input) action-set action pointer 10)
    (let ((result (make-array '(10))))
      (loop for i below 10 do (setf (aref result i)
                                    (cffi:mem-ref pointer 'vr-input-value-handle-t i))
            finally (return result)))))

@export
(defun origin-localized-name (origin &key (input *input*))
  "application to specify which parts of the origin's information it wants a string for."
  (cffi:with-foreign-string (foreign-string (make-string 512 :initial-element #\Space))
    (%get-origin-localized-name (table input) origin foreign-string 512 64)
    (cffi:foreign-string-to-lisp foreign-string)))

@export
(defun origin-tracked-device-information (input-source &key (input *input*))
  "Retrieves useful information for the origin of this action."
  (cffi:with-foreign-object (pointer '(:struct input-binding-info-t))
    (with-input-error
        (%get-origin-tracked-device-info (table input) input-source
                                         pointer
                                         (cffi:foreign-type-size '(:struct input-origin-info-t))))
    (cffi:mem-ref pointer '(:struct input-origin-info-t)))) ; works - put in render-model component name

@export
(defun action-binding-information (action &key (input *input*))
  "Retrieves useful information about the bindings for an action."
  (cffi:with-foreign-objects ((integer-pointer :uint32)
                              (origin-info-pointer '(:struct input-binding-info-t) 32))
    (with-input-error
        (%get-action-binding-info (table input) action origin-info-pointer
                                  (cffi:foreign-type-size '(:struct input-binding-info-t))
                                  32 integer-pointer))
    (let* ((returned-count (cffi:mem-ref integer-pointer :uint32))
           (binding-information (make-array (list returned-count))))
      (loop for i below returned-count
            do (setf (aref binding-information i) (cffi:mem-ref origin-info-pointer '(:struct
                                                                                      input-binding-info-t)
                                                                i))
            finally (return binding-information))))) ; needs to be improved

@export
(defun show-action-origins (action-set action &key (input *input*))
  "Shows the current binding for the action in the headset."
  (with-input-error (%show-action-origins (table input) action-set action)))

@export
(defun show-bindings-for-action-set (action-sets origin-to-highlight &key (input *input*))
  "Shows the current binding for all the actions in the specified action sets."
  (cffi:with-foreign-object (pointer '(:struct vr-active-action-set-t) (length action-sets))
    (loop for i from 0 below (length action-sets)
          do (setf (cffi:mem-ref pointer '(:struct vr-active-action-set-t))
                   (aref action-sets i)))
    (with-input-error
        (%show-bindings-for-action-set (table input) pointer
                                       (cffi:foreign-type-size '(:struct vr-active-action-set-t))
                                       (length action-sets) origin-to-highlight))))

;; legacy input

@export
(defun using-legacy-input-p (&key (input *input*))
  (%is-using-legacy-input (table input))) ; works

@export
(defclass action ()
  ((name :accessor name :initform (error "Need a name") :initarg :name)
   (handle :accessor handle :initform (error "Need a handle") :initarg :handle)
   (action-set-handle :accessor action-set-name :initarg :action-set-handle)
   (action-type :accessor action-type :initarg :action-type)))

(defmethod print-object ((object action) stream)
  (print-unreadable-object
      (object stream)
    (format stream "3B-OPENVR::ACTION ~a ~a" (name object) (handle object))))

@export
(defclass digital-action (action)
  ((action-data :accessor action-data :initform (make-instance 'input-digital-action-data))))

@export
(defclass analog-action (action)
  ((action-data :accessor action-data :initform (make-instance 'input-analog-action-data))))

@export
(defclass haptic-action (action)
  ())

@export
(defclass pose-action (action)
  ((action-data :accessor action-data :initform (make-instance 'input-pose-action-data))))

@export
(defclass skeletal-action (action)
  ((action-data :accessor action-data :initform (make-instance 'input-skeletal-action-data))
   (bones :accessor bones :initarg :bones)))

@export
(defclass bone ()
  ((name :accessor name :initarg :name)
   (index :accessor index :initarg :index)
   (parent :accessor parent :initarg :parent)))

(defmethod print-object ((object bone) stream)
  (print-unreadable-object
      (object stream)
    (format stream "3B-OPENVR::BONE ~a ~a" (name object) (index object))))


@export
(defun load-manifest (manifest-name)
  "Decodes a OpenVR action manifest file at the given pathname specifier."
  (cl-json:decode-json-from-source (pathname manifest-name)))

@export
(defun manifest-actions (manifest-name)
  "Returns a vector of actions that are contained in the manifest file."
  (map 'vector (lambda (entry)
                 (let* ((name (cdr (find :name entry :test #'eq :key #'car)))
                        (type (cdr (find :type entry :test #'eq :key #'car)))
                        (action
                          (make-instance 'action
                                         :name name
                                         :handle (action name)
                                         :action-type (intern (string-upcase type) (find-package "KEYWORD"))
                                         :action-set-handle
                                         (action-set (format nil "/actions/~a" (third (str:split "/" name)))))))
                   (case (intern (string-upcase type) (find-package "KEYWORD"))
                     (:boolean (change-class action 'digital-action))
                     ((:vector1 :vector2 :vector3) (change-class action 'analog-action))
                     (:vibration (change-class action 'haptic-action))
                     (:pose (change-class action 'pose-action))
                     (:skeleton
                      (make-skeletal-action (change-class action 'skeletal-action)))
                     (otherwise action))))
       (cdr (find :actions (load-manifest manifest-name) :test #'eq :key #'car))))

(defun make-skeletal-action (action)
  ;; (let ((bones (make-array (list (bone-count (handle action)))))
  ;;       (hierarchy (bone-hierarchy (handle action))))
  ;;   (loop for index from 0 below (length bones) do
  ;;         (setf (aref bones index)
  ;;               (make-instance 'bone :name (bone-name (handle action) index)
  ;;                                    :index index
  ;;                                    :parent (aref hierarchy index))))
  ;;   (loop for index from 0 below (length bones) do
  ;;         (setf (parent (aref bones index))
  ;;               (if (>= (aref hierarchy index) 0)
  ;;                   (aref bones (aref hierarchy index))
  ;;                   nil)))
  ;;   (setf (bones action) bones))
  (format t "a")
  action)

@export
(defun update-action-set (action-set-name)
  (update-action-state (vector (make-instance 'active-action-set
                                              :action-set-handle (action-set action-set-name)
                                              :restricted-to-device
                                              +invalid-input-value-handle+
                                              :secondary-action-set
                                              +invalid-action-set-handle+
                                              :priority 1
                                              :padding 0))))

@export
(defgeneric update-data (action))

(defmethod update-data ((action digital-action))
  (setf (action-data action) (digital-action-data (handle action))))

(defmethod update-data ((action analog-action))
  (setf (action-data action) (analog-action-data (handle action))))

(defmethod update-data ((action haptic-action))
  (values)) ;do nothing

(defmethod update-data ((action pose-action))
  (handler-case 
      (setf (action-data action)
            (pose-action-data-relative-to-now (handle action) :standing 0.0))
    (t () (progn )))) ;do nothing

(defmethod update-data ((action skeletal-action))
  ;(setf (action-data action) (skeletal-bone-data (handle action) :parent :out-controller))
  ) ;do nothing

@export
(defmethod trigger-action ((action haptic-action) from-now duration frequency amplitude)
  (trigger-haptic-vibration-action (handle action) from-now duration frequency amplitude +invalid-input-value-handle+))

@export
(defmethod skeletal-tracking-level ((action skeletal-action))
  (skeletal-tracking-level-for-handle (handle action)))

(defmethod cffi:translate-from-foreign :around (value (type vr-skeletal-summary-data-t-tclass))
  (let ((ret-val (call-next-method)))
    (setf (finger-curl ret-val)
          (map 'vector
               (lambda (index) (cffi:mem-aref (finger-curl ret-val) :float index))
               #(0 1 2 3 4))) ; 5 fingers
    (setf (finger-splay ret-val)
          (map 'vector
               (lambda (index) (cffi:mem-aref (finger-splay ret-val) :float index))
               #(0 1 2 3)))
    ret-val))

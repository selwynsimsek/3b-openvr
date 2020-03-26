;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the IVRDebug API.

;;; IVRDebug_001

(in-package 3b-openvr)



(defmacro with-debug-error (&rest body)
  (let ((error-name (gensym "error-value")))
    `(let ((,error-name (progn ,@body)))
       (if (eq ,error-name :success) t (error "VR debug error: ~a" ,error-name)))))


(defun emit-profiler-event (message &key (debug *debug*))
  "Create a vr profiler discrete event (point). The event will be associated with the message 
   provided in message, and the current time will be used as the event timestamp."
  (with-debug-error
      (%emit-vr-profiler-event (table debug) message))) ; appears to work


(defun begin-profiler-event (&key (debug *debug*))
  "Create an vr profiler duration event (line). The current time will be used as the timestamp for 
   the start of the line. On success, returns a handle valid for terminating this event."
  (cffi:with-foreign-object (handle-pointer 'vr-profiler-event-handle-t)
    (with-debug-error
        (%begin-vr-profiler-event (table debug) handle-pointer))
    (cffi:mem-ref handle-pointer 'vr-profiler-event-handle-t))) ; appears to work


(defun finish-profiler-event (profiler-event message &key (debug *debug*))
  "Terminate a vr profiler event. The event associated with profiler-event-handle will be considered
   completed when this method is called. The current time will be used as the termination time of
   the event, and message will be used as the event title."
  (with-debug-error
      (%finish-vr-profiler-event (table debug) profiler-event message))) ; appears to work


(defun driver-debug-request (tracked-device-index request &key (debug *debug*)
                                                               (max-buffer-size 2048))
  "Sends a request to the driver for the specified device and returns the response."
  (cffi:with-foreign-string (foreign-string (make-string max-buffer-size))
    (%driver-debug-request (table debug)
                           tracked-device-index
                           request
                           foreign-string
                           max-buffer-size)
    (cffi:foreign-string-to-lisp foreign-string))) ; appears to work

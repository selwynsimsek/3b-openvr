;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the IVRDebug API.

;;; IVRDebug_001

(defun emit-profiler-event (message &key (debug *debug*))
  "Create a vr profiler discrete event (point). The event will be associated with the message provided in message, and the current time will be used as the event timestamp.")

(defun begin-profiler-event (profiler-event-handle &key (debug *debug*))
  "Create an vr profiler duration event (line). The current time will be used as the timestamp for the start of the line. On success, returns a handle valid for terminating this event.")

(defun finish-profiler-event (profiler-event-handle message &key (debug *debug*))
  "Terminate a vr profiler event.
  The event associated with profiler-event-handle will be considered completed when this method is called. The current time will be used as the termination time of the event, and message will be used as the event title.")

(defun driver-debug-request (tracked-device-index request &key (debug *debug*))
  "Sends a request to the driver for the specified device and returns the response.")

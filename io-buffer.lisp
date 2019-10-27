;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the IVRIOBuffer API.
;;; https://github.com/ValveSoftware/openvr/wiki/IVRIOBuffer

;;; IVR_IOBuffer_002

;; The IVRIOBuffer interface supports creating, reading from, and writing to a fixed-element-size cross-process, globally-named ring-buffer with extremely low synchronization overhead between multiple readers and a single writer. This interface is useful whenever there is very high-frequency of high-bandwidth data stream where it is acceptable for readers to fall behind and even miss elements in the stream, as this allows the writer to use a ring-buffer instead of an ever-growing buffer, and the writer can avoid writing if there are no readers.
(in-package 3b-openvr)

;(defclass io-buffer ()) ; gray streams for this one?

(defun open-io-buffer ())

(defun close-io-buffer ())

(defun read-from-buffer ())



(defun property-container ())

(defun has-readers-p (&key (io-buffer *io-buffer*)))

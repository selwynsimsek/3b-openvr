;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level bindings for the IVRIOBuffer API.
;;; https://github.com/ValveSoftware/openvr/wiki/IVRIOBuffer

;;; IVR_IOBuffer_002

;; The IVRIOBuffer interface supports creating, reading from, and writing to a fixed-element-size
;; cross-process, globally-named ring-buffer with extremely low synchronization overhead between
;; multiple readers and a single writer. This interface is useful whenever there is very
;; high-frequency of high-bandwidth data stream where it is acceptable for readers to fall behind
;; and even miss elements in the stream, as this allows the writer to use a ring-buffer instead of
;; an ever-growing buffer, and the writer can avoid writing if there are no readers.

(in-package 3b-openvr)

(defclass io-buffer () ; gray streams for this one?
  ((handle :initarg :handle :accessor handle)
   (path :initarg :path :accessor path)
   (mode :initarg :mode :accessor mode)
   (element-size :initarg :element-size :accessor element-size)
   (number-of-elements :initarg :number-of-elements :accessor number-of-elements)))

(defun open-io-buffer (path mode element-size number-of-elements &key (io-buffer *io-buffer*))
  (cffi:with-foreign-object (pointer 'obuffer-handle-t)
    (let ((error-code
            (%open (table io-buffer) path mode element-size number-of-elements pointer)))
      (unless (eq error-code :none)
        (error "IO Buffer error: ~a" error-code))
      (make-instance 'io-buffer
                     :handle (cffi:mem-ref pointer 'obuffer-handle-t)
                     :path path
                     :mode mode
                     :element-size element-size
                     :number-of-elements number-of-elements))))

(defun close-io-buffer (buffer &key (io-buffer *io-buffer*))
  (let ((error-code (%close (table io-buffer) (handle buffer))))
    (unless (eq error-code :none)
      (error "IO Buffer error: ~a" error-code))))

(defun read-from-buffer (buffer destination &key (io-buffer *io-buffer*))
  (error "implement me"))

(defun write-to-buffer (buffer source &key (io-buffer *io-buffer*))
  (error "implement me"))


(defun property-container (buffer &key (io-buffer *io-buffer*))
  "Retrieves the property container of an buffer."
  (%property-container (table io-buffer) (handle buffer)))

(defun has-readers-p (buffer &key (io-buffer *io-buffer*))
  "Inexpensively checks for readers to allow writers to fast-fail potentially expensive copies and
   writes."
  (%has-readers (table io-buffer) (handle buffer)))

(export '(has-readers-p property-container io-buffer open-io-buffer close-io-buffer))

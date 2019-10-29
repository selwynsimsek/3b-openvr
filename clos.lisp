(in-package 3b-openvr)

(defmacro define-clos-wrapper ((class-name struct-name) direct-superclasses slot-names &rest options)
  (let ((class-slots (mapcar #'first slot-names))
        (cstruct-slots (mapcar #'second slot-names))
        (struct-type (intern (concatenate 'string (princ-to-string struct-name) "-TCLASS"))))
    `(progn
       (defclass ,class-name ,direct-superclasses
         ,(mapcar
           (lambda (class-slot)
             `(,class-slot :initarg ,(intern (princ-to-string class-slot) (find-package "KEYWORD"))
                           :accessor ,class-slot))
           class-slots)
         ,@options)
       (defmethod cffi:translate-from-foreign (value (type ,struct-type))
         (cffi:with-foreign-slots (,cstruct-slots value (:struct ,struct-name))
           (make-instance ',class-name
                          ,@(loop for (class-slot cstruct-slot) in slot-names
                                  appending (list (intern (princ-to-string class-slot) (find-package "KEYWORD"))
                                                  cstruct-slot)))))
       (defmethod cffi:translate-to-foreign (value (type ,struct-type))
         (let ((pointer (cffi:foreign-alloc '(:struct ,struct-name))))
           (cffi:with-foreign-slots (,cstruct-slots pointer (:struct ,struct-name))
             ,@(loop for (class-slot cstruct-slot) in slot-names
                     collecting `(setf ,cstruct-slot (slot-value value ',class-slot)))
             pointer)))
       (defmethod cffi:translate-into-foreign-memory (value (type ,struct-type) pointer)
         (cffi:with-foreign-slots (,cstruct-slots pointer (:struct ,struct-name))
           ,@(loop for (class-slot cstruct-slot) in slot-names
                   collecting `(setf ,cstruct-slot (slot-value value ',class-slot))))))))

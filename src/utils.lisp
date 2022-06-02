(in-package :cl-user)
(defpackage scaffold.utils
  (:use :cl))

(in-package :scaffold.utils)

(defun defword ()
  (make-array 0
              :element-type 'character
              :fill-pointer 0
              :adjustable t))

(defmacro concat (string &rest words)
  `(setf ,string (concatenate 'string ,string ,@words)))




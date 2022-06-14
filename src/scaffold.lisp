(in-package :cl-user)
(defpackage scaffold
  (:use :cl
   :scaffold.utils)
  (:export :read-template))

(in-package :scaffold)

(defparameter *specials* '(("#/" . in-file) ("#:" . set-binds)))
(defparameter *binds* nil)
(defparameter *shapers* '(#\: #\-))
(defvar *file* nil)
(defvar *project-root* nil)

(defun set-root (&optional dir)
  (handler-case 
      (let ((curr (or dir (uiop/os:getcwd))))
        (if (some #'(lambda (x) (search ".git" (namestring x))) (ls curr))
            (setf *project-root* curr)
            (set-root (upper-directory curr))))
    (error () (format t "Cannot find root directory"))))

(defun relative-path (file)
  (merge-with-dir file *project-root*))

(defun in-file (file)
  (when (dirp file)
    (mkdir (relative-path (upper-directory file))))
  (setf *file* (relative-path file)))

(defun set-binds (binds)
  (setf *binds* (read-from-string binds)))

(defun shaper (char)
  (some #'(lambda (c) (char= char c)) *shapers*))

(defun scaffold (string)
  (let ((result ""))
    (loop for c across string
          with word = (defword)
          if (or (alphanumericp c) (shaper c))
            do (vector-push-extend c word)
          else
            do (progn
                 (concat result (when (string/= word "") (or (bind word) word)) (string c))
                 (setf word (defword)))
          finally (concat result (or (bind word) word)))    
    result))

(defun bind (word)
  (cdr (assoc word *binds* :test #'string=)))

(defun read-template (file)
  (set-root)
;  (handler-case       
      (with-open-file (stream file)
        (loop for line = (read-line stream nil)
              while line
              if (parse-line line)
                do (write-template line)))
  ;(error (e) (print e))
  )

(defun parse-line (line)
  (let* ((l (nth-value 1 (cl-ppcre:scan-to-strings "(^.*?)\\s(.*)" line)))
         (special (when l (cdr (assoc (elt l 0) *specials* :test #'string=)))))
    (if special
        (progn (funcall special (elt l 1))
               nil)
        line)))

(defun write-template (template)
  (with-open-file (stream *file* :direction :output :if-exists :append :if-does-not-exist :create)
  (format stream "~&~a~%" (scaffold template))))

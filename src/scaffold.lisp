(in-package :cl-user)
(defpackage scaffold
  (:use :cl
   :scaffold.utils)
  (:export :read-template))

(in-package :scaffold)

(defparameter *specials* '(("#/" . in-file)))
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
    (print (mkdir (relative-path (upper-directory file)))))
  (print (setf *file* (relative-path file))))

(defun scaffold (string binds)
  (let ((result ""))
    (loop for c across string
          with word = (defword)
          if (alphanumericp c)
            do (vector-push-extend c word)
          else
            do (progn
                 (concat result (or (bind word binds) word) (string c))
                 (setf word (defword))))    
    result))

(defun bind(word binds)
  (cdr (assoc (intern (string-upcase word)) binds)))

(defun read-template (file)
  (set-root)
  (handler-case       
      (with-open-file (stream "~/test/scaffold.temp")
        (loop for line = (read-line stream nil)
              while line
              if (parse-line line)
                do (write-template line nil)))
    (error (e) (print e))))

(defun parse-line (line)
  (let* ((l (cl-ppcre:split "\\s" line))
         (special (cdr (assoc (car l) *specials* :test #'string=))))
    (if special
        (progn (funcall special (cadr l))
               nil)
        line)))

(defun write-template (template binds)
  (with-open-file (stream *file* :direction :output :if-exists :append :if-does-not-exist :create)
  (format stream "~&~a~%" (scaffold template binds))))

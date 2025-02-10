(defpackage scaffold
  (:use :cl
        :scaffold.utils
        :scaffold.parser)
  (:export :main))

(in-package :scaffold)

(defvar *config* "~/.config/scaffold/config.lisp")
(defvar *default-templates-path* "~/scaffold-templates/")

(defparameter *settings* nil)

(defun find-template (name)
  (or (probe-file name)
      (loop for template in (ls *default-templates-path*)
            if (search name (namestring template))
              return template)))

(defun set-root (&optional dir)
  (handler-case 
      (let ((curr (or dir (uiop/os:getcwd))))
        (if (some #'(lambda (x) (search ".git" (namestring x))) (ls curr))
            (setf *project-root* curr)
            (set-root (upper-directory curr))))
    (error () (format t "Cannot find root directory"))))

(defun scaffold (template args)  
  (with-open-file (stream template)
    (set-binds (read-line stream nil))
    (loop for line = (read-line stream nil)
          while line
          do (make-line (parse-line line)))))

(defun make-line (template)
  (when template
    (with-open-file (stream *file* :direction :output :if-exists :append :if-does-not-exist :create)
      (write-line (replace-bindings template) stream))))

(defun main (template args)
  (set-root)
  (setf *args* args)
  (scaffold (find-template template) args))

;; config?

(defun parse-settings (file)
  (let ((settings (make-hash-table :test #'equalp)))
    (with-open-file (stream file)
      (loop with regexp = nil
            for line = (read-line stream nil)
            while line
            do (setf regexp (nth-value 1 (cl-ppcre:scan-to-strings "(.*)=(.*)" line)))
            do (sethash (elt regexp 0)
                        (elt regexp 1)
                        settings)))
    (setf *settings* settings)))

(defun setting (key)
  (gethash key *settings*))

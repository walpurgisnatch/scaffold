(in-package :cl-user)
(defpackage scaffold
  (:use :cl
        :scaffold.utils))

(in-package :scaffold)

(defun break-words (string binds)
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
  (with-open-file (stream "~/test/scaffold.temp")
    (loop for line = (read-line stream nil)
          with curr-path = ""
          with new-path = ""
          while line
          do (loop for c across line
                   with word = (defword)
                   do (vector-push-extend c word)
                   when (string= word "#/")
                     do (progn (setf new-path "")
                               (setf word ""))                                            
                   do (vector-push-extend c new-path)))))
             

(defun write-template (template binds)
  (with-open-file (stream "~/test/scaffold" :direction :output :if-exists :append :if-does-not-exist :create)
  (format stream "~%~a~%" (break-words template binds))))

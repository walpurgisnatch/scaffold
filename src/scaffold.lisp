(in-package :cl-user)
(defpackage scaffold
  (:use :cl
   :scaffold.utils)
  (:export :read-template
           :find-template))

(in-package :scaffold)

(defparameter *specials* '(("#/" . in-file) ("#:" . set-binds)))
(defparameter *args* nil)
(defparameter *binds* nil)
(defparameter *shapers* '(#\: #\- #\#))
(defvar *file* nil)
(defvar *project-root* nil)
(defvar *config* "~/.config/scaffold/config.lisp")
(defvar *templates* "~/templates/")

(defun find-template (name)
  (loop for template in (ls *templates*)
        if (search name (namestring template))
          return template))

(defun read-config ()
  (setf *templates*
        (with-open-file (config *config*)
          (elt
           (nth-value 1 (cl-ppcre:scan-to-strings "templates_path: (.*)"
                                                  (read-line config)))
           0))))

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
  (setf *binds* (parse-binds (brake-words binds))))

(defun parse-binds (binds &optional (args *args*) type result)
  (let ((bind (car binds))
        (arg (car args)))
    (cond ((null arg)
           result)
          ((string-starts-with bind "&")
           (parse-binds (cdr binds) args type (acons (subseq bind 1) nil result)))
          ((string-starts-with arg "-")
           (parse-binds binds (cdr args) (subseq arg 1) result))
          ((null type)
           (parse-binds (cdr binds) (cdr args) nil (acons bind arg result)))
          ((null (assoc type result :test #'string=))
           (format t "missing arguments"))
          (t (nconc (assoc type result :test #'string=) (list arg))
             (parse-binds binds (cdr args) type result)))))

(defun shaper (char)
  (some #'(lambda (c) (char= char c)) *shapers*))

(defun brake-words (line)
  (remove-if #'(lambda (x) (string= x "")) (cl-ppcre:all-matches-as-strings "[^\\s]*" line)))

(defun scaffold (string &optional binds)
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

(defun bind (word &optional (binds *binds*))
  (let ((w (nth-value 1 (cl-ppcre:scan-to-strings "##(.*)" word))))
    (if w
        (cdr (assoc (elt w 0) binds :test #'string=))
        word)))

(defun read-template (file args)
  (read-config)
  (set-root)
  (setf *args* args)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          if (parse-line line)
            do (write-template (parse-line line)))))

(defun parse-line (line)
  (let* ((l (nth-value 1 (cl-ppcre:scan-to-strings "(^.*?)\\s(.*)" line)))
         (special (when l (cdr (assoc (elt l 0) *specials* :test #'string=))))
         (sblock (nth-value 1 (cl-ppcre:scan-to-strings "(.*?)#<.*(~{.*?~})(.*)>#(.*)" line)))
         (block-binds-strings (when sblock (cl-ppcre:all-matches-as-strings "&[^\\s]*" (elt sblock 2))))
         (block-binds (loop for bind in block-binds-strings
                            collect (cdr (assoc (subseq bind 1) *binds* :test #'string=)))))
    (if sblock (format-string (elt sblock 0) (elt sblock 1) block-binds (elt sblock 3))
        (if special
            (progn (funcall special (elt l 1))
                   nil)
            line))))

(defun format-string (beg block vars end)
  (let* ((ident (length beg))
        (string (cl-ppcre:regex-replace-all "~%" block (format nil "~%~va" ident " "))))
    (concatenate 'string
                 beg
                 (format nil (concatenate 'string "~{" string "~}") vars)
                 end)))

(defun write-template (template)
  (with-open-file (stream *file* :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "~&~a~%" (scaffold template))))

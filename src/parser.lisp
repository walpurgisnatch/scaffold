(defpackage scaffold.parser
  (:use :cl
        :scaffold.utils)
  (:export :*args*
           :*binds*
           :*file*
           :*project-root*
           :replace-bindings
           :parse-line
           :set-binds))

(in-package :scaffold.parser)

(defvar *file* nil)
(defvar *project-root* nil)

(defparameter *args* nil)
(defparameter *binds* nil)
(defparameter *shapers* '(#\: #\- #\# #\<))
(defparameter *change-file-pattern* "#/")
(defparameter *bind-start-pattern* "#<")
(defparameter *bind-end-pattern* "#>")

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

(defun set-binds (binds)
  (setf *binds* (parse-binds (words binds))))

(defun shaperp (char)
  (some #'(lambda (c) (char= char c)) *bind-start-symbols*))

(defun in-file (template)
  (let ((file (replace-bindings template)))
    (when (dirp file)
      (mkdir (relative-path (upper-directory file) *project-root*)))
    (setf *file* (relative-path file *project-root*))
    nil))

(defun format-stringp (string)
  (search "#(" string))

(defun bind (var binds &optional ident)
  (if (format-stringp var)
      (let* ((line (string-trim '(#\Space) var))
             (start (search "#(" line))
             (end (search ")#" line))
             (format-string (subseq line (+ start 2) end))
             (vars (subseq line (+ end 3))))
        (format-string ident format-string (assoc-str (subseq vars 1) binds)))
      (assoc-str var binds)))

(defun format-string (ident block vars)
  (let ((string (cl-ppcre:regex-replace-all "~%" block (format nil "~%~va" ident " "))))
    (format nil string vars)))

(defun replace-bindings (string &optional (binds *binds*))
  (let ((result (make-array '(0) :element-type 'base-char
                                 :fill-pointer 0 :adjustable t))
        (pos 0))
    (with-output-to-string (output result)
      (loop for start = (search *bind-start-pattern* string :start2 pos)
            while start
            do (let* ((end (search *bind-end-pattern* string
                                   :start2 (+ start (length *bind-start-pattern*))))
                      (key (subseq string (+ start (length *bind-start-pattern*)) end))
                      (replacement (bind key binds start)))
                 (format output "~a~a" (subseq string pos start) (or replacement ""))
                 (setf pos (+ end 2))))
      (write-string (subseq string pos) output))
    result))

(defun parse-line (line)
  (let ((file-changed (and (> (length line) (length *change-file-pattern*))
                           (string-starts-with line *change-file-pattern*))))
    (if file-changed
        (in-file (subseq line (1+ (length *change-file-pattern*))))
        line)))

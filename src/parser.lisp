(defpackage scaffold.parser
  (:use :cl
        :scaffold.utils)
  (:export :*args*
           :*binds*
           :*file*
           :*project-root*
           :replace-bindings
           :parse-line))

(in-package :scaffold.parser)

(defvar *file* nil)
(defvar *project-root* nil)

(defparameter *specials* '(("#/" . in-file) ("#:" . set-binds)))
(defparameter *args* nil)
(defparameter *binds* nil)
(defparameter *shapers* '(#\: #\- #\# #\<))
(defparameter *bind-start-symbols* '(#\# #\<))
(defparameter *bind-end-symbol* #\>)

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

(defun parse-args (line)
  )

(defun set-binds (binds)
  (setf *binds* (parse-binds (words binds))))

(defun shaperp (char)
  (some #'(lambda (c) (char= char c)) *bind-start-symbols*))

(defun end-bindp (char)
  (char= char *bind-end-symbol*))

(defun start-bindp (string)
  (and (= 2 (length string))
       (string= string (concatenate 'string *bind-start-symbols*))))

(defun in-file (template)
  (let ((file (replace-bindings template)))
    (when (dirp file)
      (mkdir (relative-path (upper-directory file) *project-root*)))
    (setf *file* (relative-path file *project-root*))))

(defun bind (word binds)
  (let ((w (group-first "^#<(.*)>$" word)))
    (if (and w (not-emptyp w)) (assoc-str w binds) word)))

(defun format-string (beg block vars end)
  (let* ((ident (length beg))
        (string (cl-ppcre:regex-replace-all "~%" block (format nil "~%~va" ident " "))))
    (concatenate 'string
                 beg
                 (format nil (concatenate 'string "~{" string "~}") vars)
                 end)))

;; (defun replace-bindings (string &optional (binds *binds*))
;;   (let ((result ""))
;;     (loop for c across string
;;           with word = (defword)
;;           with binded = nil
;;           if (and (shaperp c) (< 2 (length word)))
;;             do (vector-push-extend c word)
;;                if (string= word (concatenate 'string *bind-end-symbol*))
                 
;;           else
;;             do (progn (concat result (when (not-emptyp word) (bind word binds))
;;                               (string c))
;;                       (setf word (defword)))
;;           finally (concat result (bind word binds)))
;;     result))

(defun replace-bindings (string &optional (binds *binds*))
  (labels ((repl (list &optional result word binded)
             (let ((c (car list)))
               (and c (vector-push-extend c word))
               (print word)
               (cond ((null c) result)
                     ((and binded (end-bindp c))
                      (repl (cdr list) (concat result (bind word binds)) (defword)))
                     (binded
                      (repl (cdr list) result word t))
                     ((and (shaperp c) (start-bindp word))
                      (repl (cdr list) result word t))
                     ((and (shaperp c) (< (length word) 2))
                      (repl (cdr list) result word binded))
                     (t (repl (cdr list) (concat result word) (defword) binded))))))
    (repl (coerce string 'list) "" (defword))))

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

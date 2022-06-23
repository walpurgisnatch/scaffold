(in-package :cl-user)
(defpackage scaffold.utils
  (:use :cl)
  (:export :defword
           :ls
           :upper-directory
           :dirp
           :mkdir
           :merge-with-dir
           :concat
           :string-starts-with))

(in-package :scaffold.utils)

(defun defword ()
  (make-array 0
              :element-type 'character
              :fill-pointer 0
              :adjustable t))

(defun string-starts-with (string x)
  (string-equal string x :end1 (length x)))

(defmacro concat (string &rest words)
  `(setf ,string (concatenate 'string ,string ,@words)))

(defun directory-wildcard (dir)
  (make-pathname
   :name :wild
   :type :wild
   :defaults (pathname-as-directory dir)))

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p  (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name      nil
         :type      nil
         :defaults pathname)
        pathname)))

(defun ls (&optional directory)
  (let ((dir (or directory ".")))
    (when (wild-pathname-p dir)
      (error "Wildcard not supported"))
    (directory (directory-wildcard dir))))

(defun dirp (path)
  (find #\/ path))

(defun upper-directory (&optional directory)
  (elt (nth-value 1 (cl-ppcre:scan-to-strings "(.*/).+$" (namestring (or directory (uiop/os:getcwd))))) 0))

(defun mkdir (dir &optional parent)
  (namestring (ensure-directories-exist
               (if parent
                   (merge-with-dir dir parent)
                   (pathname-as-directory dir)))))

(defun merge-with-dir (child parent)
  (merge-pathnames child (pathname-as-directory parent)))

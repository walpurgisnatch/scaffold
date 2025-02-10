(defpackage scaffold/tests/main
  (:use :cl
        :scaffold
        :fiveam)
  (:export :scaffold))

(in-package :scaffold/tests/main)

(setf *on-failure* nil)
(setf *on-error* :debug)

(def-suite* scaffold
  :description "Scaffold tests")

(defparameter first-file
  "(make-some-staff :folder foldername
   :clear t
   :done t)")

(defparameter second-file
  "(create :name filename
        :username username
        :date date
        :created_at (local-time:now))")

(defun read-file (file)
  (string-trim '(#\Newline) (alexandria:read-file-into-string file)))

(test scaffold
  (scaffold:main "../tests/test.temp" '("foldername" "filename" "-vars" "username" "date" "-functions" "clear"))
  (is-true (probe-file "../predefined/functions.lisp"))
  (is-true (probe-file "../foldername/filename.t"))
  (is (string= first-file (read-file "../predefined/functions.lisp")))
  (is (string= second-file (read-file "../foldername/filename.t")))
  (uiop:delete-directory-tree (truename "../predefined")
                              :validate (lambda (path)
                                          (uiop:string-prefix-p "/home/vic/cl/scaffold/"
                                                                (uiop:native-namestring path))))
  (uiop:delete-directory-tree (truename "../foldername")
                              :validate (lambda (path)
                                          (uiop:string-prefix-p "/home/vic/cl/scaffold/"
                                                                (uiop:native-namestring path)))))


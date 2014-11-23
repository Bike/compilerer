(in-package #:compilerer)

(defun exec (thunk)
  (funcall thunk nil))

(defun compile-form* (form)
  (compile-form form nil))

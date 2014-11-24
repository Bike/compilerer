(in-package #:compilerer)

(defun exec (thunk stack)
  (funcall thunk stack))

(defun exec* (thunk)
  (exec thunk nil))

(defun compile-form* (form)
  (compile-form form nil))

(in-package #:compilerer)

(defun initarg-not-provided ()
  (error "Internal bug: tried to make a struct without providing arg"))

(defstruct lexenv
  (parent (initarg-not-provided) :type (or lexenv null)))

(deftype empty-lexenv () 'null)

(defmacro climbing-lexenv (lexenv count &body body)
  `(loop for ,lexenv = ,lexenv then (lexenv-parent ,lexenv)
      for ,count from 0
      do (etypecase ,lexenv ,@body)))

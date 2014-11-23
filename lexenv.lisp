(in-package #:compilerer)

(defstruct lexenv
  (parent :type lexenv))

(defstruct (empty-lexenv (:include lexenv)))

(defmacro climbing-lexenv (lexenv count &body body)
  `(loop for ,lexenv = ,lexenv then (lexenv-parent ,lexenv)
      for ,count from 0
      do (etypecase ,lexenv ,@body)))

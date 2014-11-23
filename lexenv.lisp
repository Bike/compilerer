(in-package #:compilerer)

(defun initarg-not-provided ()
  (error "Internal bug: tried to make a struct without providing arg"))

(defstruct lexenv
  (parent (initarg-not-provided) :type (or lexenv null)))

(deftype empty-lexenv () 'null)

(deftype lexenv-designator () '(or lexenv empty-lexenv))

(defstruct (compiler-lexenv (:include lexenv))
  "A lexenv that doesn't have runtime frames, e.g. macro environments.")

(defmacro climbing-lexenv (lexenv &body body)
  (let ((senv (gensym "ENV")))
    `(let ((,senv ,lexenv))
       (loop for ,lexenv = ,senv then (lexenv-parent ,lexenv)
	  do (etypecase ,lexenv ,@body)))))

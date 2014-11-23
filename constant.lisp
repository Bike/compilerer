(in-package #:compilerer)

(defmethod compile-form (form lexenv) ; default
  (compile-constant form))

(defmethod compile-cons ((operator (eql 'quote)) operands lexenv)
  (assert (= (length operands) 1))
  (compile-quote (first operands) lexenv))

(defun compile-constant (constant)
  (lambda (frame) (declare (ignore frame)) constant))

(defun compile-quote (thing lexenv)
  (declare (ignore lexenv))
  (compile-constant thing))

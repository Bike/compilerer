(in-package #:compilerer)

(deftype function-name ()
  '(or symbol (cons (eql setf) (cons symbol null))))

(defmethod compile-cons ((operator (eql 'function)) operands lexenv)
  (assert (= (length operands) 1))
  (compile-function (first operands) lexenv))

(defun compile-function (thing lexenv)
  (etypecase thing
    (function-name (compile-flookup thing lexenv))
    ((cons (eql lambda))
     (compile-lambda (second thing) (cddr thing) lexenv))))

(defun compile-flookup (name lexenv)
  ;; TODO: flet/labels
  (declare (ignore lexenv))
  (lambda (stack)
    (declare (ignore stack))
    (fdefinition name)))

(in-package #:compilerer)

(defmethod compile-cons ((operator (eql 'if)) operands lexenv)
  (assert (= (length operands) 3))
  (compile-if (first operands) (second operands) (third operands)
	      lexenv))

(defun compile-if (test then else lexenv)
  (let ((test (compile-form test lexenv))
	(then (compile-form then lexenv))
	(else (compile-form else lexenv)))
    (lambda (stack)
      (if (funcall test stack)
	  (funcall then stack)
	  (funcall else stack)))))

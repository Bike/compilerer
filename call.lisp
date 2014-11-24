(in-package #:compilerer)

(defmethod compile-cons ((operator symbol) operands lexenv) ; default
  (compile-call operator operands lexenv))

(defmethod compile-cons ((operator cons) operands lexenv)
  ;; lambda form
  (compile-call operator operands lexenv))

(defmethod compile-cons ((operator (eql 'multiple-value-call))
			 operands lexenv)
  (compile-multiple-value-call (first operands) (rest operands) lexenv))

(defun compile-call (function arguments lexenv)
  (let ((function (etypecase function
		    (symbol (compile-flookup function lexenv))
		    ((cons (eql lambda))
		     (compile-lambda (second function) (cddr function)
				     lexenv))))
	(arguments (mapcar (lambda (a) (compile-form a lexenv)) arguments)))
    (lambda (stack)
      (apply (funcall function stack)
	     (loop for a in arguments collecting (funcall a stack))))))

(defun compile-multiple-value-call (function arguments lexenv)
  (let ((function (compile-form function lexenv))
	(arguments (mapcar (lambda (a) (compile-form a lexenv)) arguments)))
    (lambda (stack)
      (apply (funcall function stack)
	     (loop for a in arguments
		appending (multiple-value-list (funcall a stack)))))))

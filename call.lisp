(in-package #:compilerer)

;;; TODO: lambda forms

(defmethod compile-cons ((operator symbol) operands lexenv) ; default
  (compile-call operator operands lexenv))

(defmethod compile-cons ((operator (eql 'multiple-value-call))
			 operands lexenv)
  (compile-multiple-value-call (first operands) (rest operands) lexenv))

(defun compile-call (function arguments lexenv)
  (let ((function (compile-flookup function lexenv))
	(arguments (mapcar (lambda (a) (compile-form a lexenv)) arguments)))
    (lambda (frame)
      (apply (funcall function frame)
	     (loop for a in arguments collecting (funcall a frame))))))

(defun compile-multiple-value-call (function arguments lexenv)
  (let ((function (compile-form function lexenv))
	(arguments (mapcar (lambda (a) (compile-form a lexenv)) arguments)))
    (lambda (frame)
      (apply (funcall function frame)
	     (loop for a in arguments
		appending (multiple-value-list (funcall a frame)))))))

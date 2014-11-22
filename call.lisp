(defun compile-call (function arguments lexenv)
  (let ((function (compile-flookup function lexenv))
	(arguments (mapcar (lambda (a) (compile-form a lexenv)) arguments)))
    (lambda (frame)
      (apply (funcall function frame)
	     (loop for a in arguments collecting (funcall a frame))))))

(defun compile-multiple-value-call (function arguments lexenv)
  (let ((function (compile-flookup function lexenv))
	(arguments (mapcar (lambda (a) (compile-form a lexenv)) arguments)))
    (lambda (frame)
      (apply (funcall function frame)
	     (loop for a in arguments
		appending (multiple-value-list (funcall a frame)))))))

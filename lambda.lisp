(in-package #:compilerer)

(defun compile-lambda (llist body lexenv)
  (multiple-value-bind (body decls doc) (parse-body body :documentation t)
    (declare (ignore doc))
    (if (or decls
	    (some (lambda (s) (specialp s lexenv)) llist))
	(error "lambda fanciness not supported yet")
	(compile-simple-lambda llist body lexenv))))

(defun compile-simple-lambda (llist body lexenv)
  "Lambda with only lexical variables and no &keywords."
  ;; partly because it's a common case, partly practice
  ;; also no decls
  (let* ((len (length llist))
	 (env (make-simple-var-lexenv :parent lexenv :vars llist))
	 (body (compile-progn body env)))
    (lambda (frame)
      ;; this thunk constructs a host lisp lambda.
      (lambda (&rest args)
	(funcall body (cons (make-array len :initial-contents args) frame))))))

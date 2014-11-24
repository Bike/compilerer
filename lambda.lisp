(in-package #:compilerer)

;;; TODO: specials

(defun compile-lambda (llist body lexenv)
  (multiple-value-bind (body decls doc) (parse-body body :documentation t)
    (multiple-value-bind (req opt rest key aok-p aux key-p)
	(parse-ordinary-lambda-list llist)
      (let ((result
	     (cond ((or decls
			(some (lambda (s) (specialp s lexenv)) llist))
		    (error "lambda specials not supported yet"))
		   ((or opt rest key aok-p aux key-p)
		    (compile-complex-lambda req opt rest key aok-p
					    aux key-p body lexenv))
		   (t (compile-simple-lambda req body lexenv)))))
	(prog1
	    result
	  ;; doesn't always work, but we go for it anyway.
	  (setf (documentation result 'function) doc))))))

(defun compile-simple-lambda (llist body lexenv)
  "Lambda with only lexical variables and no &keywords."
  ;; partly because it's a common case, partly practice
  ;; also no decls
  (let* ((len (length llist))
	 (env (make-simple-var-lexenv :parent lexenv :vars llist))
	 (body (compile-progn body env)))
    (lambda (stack)
      ;; this thunk constructs a host lisp lambda.
      (lambda (&rest args)
	(funcall body (cons (make-array len :initial-contents args)
			    stack))))))

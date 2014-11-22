(defun compile-form (form lexenv)
  (let ((form (macroexpand form lexenv)))
    (typecase form
      (symbol (compile-symbol form lexenv))
      (cons (etypecase (car form)
	      ((cons (eql lambda)) (compile-lambda-form form lexenv))
	      ((satisfies special-operator-p) (compile-special-form form lexenv))
	      (symbol (compile-call form lexenv))))
      (t (compile-constant form)))))

(defun compile-special-form (form lexenv)


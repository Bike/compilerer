(defun lookup-symbol (symbol lexenv)
  ;; symbol is guaranteed already macroexpanded
  (loop for lexenv = lexenv then (lexenv-parent lexenv)
     for i from 0
     when (empty-lexenv-p lexenv)
     ;; toplevel constants and stuff actually. boo.
     do (warn "unbound variable ~a" symbol) (return 'special)
     when (and (svar-lexenv-p lexenv) (find symbol (svar-lexenv-vars lexenv)))
     return 'special
     when (lvar-lexenv-p lexenv)
     do (let ((maybe (position symbol (lvar-lexenv-vars lexenv))))
	  (when maybe
	    (return (values lexenv maybe i))))))

(defun compile-symbol (symbol lexenv)
  (multiple-value-bind (env pos depth)
      (lookup-symbol symbol lexenv)
    (if (eq env 'special)
	(compile-special symbol)
	(compile-lexical pos depth))))

(defun compile-special (symbol)
  (lambda (frame)
    (declare (ignore frame))
    (symbol-value symbol)))

(defun compile-lexical (pos depth)
  (lambda (frame)
    (aref (nth depth frame) pos)))

(defun compile-setq (things lexenv)
  (flet ((compile-%setq (place value)
	   (multiple-value-bind (form expanded?)
	       (macroexpand-1 place lexenv)
	     (if expanded? ;; at all, including into another symbol, i guess?
		 (compile-form `(setf ,form ,value) lexenv)
		 (multiple-value-bind (env pos depth)
		     (lookup-symbol place lexenv)
		   (if (eq env 'special)
		       (compile-special-setq place value lexenv)
		       (compile-lexical-setq pos depth value lexenv)))))))
    (compile-seq (loop for (place value) on things by #'cddr
		    collecting (compile-%setq place value)))))

(defun compile-special-setq (place value lexenv)
  (let ((value (compile-form value lexenv)))
    (lambda (frame)
      (setf (symbol-value place) (funcall value frame)))))

(defun compile-lexical-setq (pos depth value lexenv)
  (let ((value (compile-form value lexenv)))
    (lambda (frame)
      (setf (aref (nth depth frame) pos) (funcall value frame)))))

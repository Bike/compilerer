(in-package #:compilerer)

(defmethod compile-form ((form symbol) lexenv)
  (compile-symbol form lexenv))

(defmethod compile-cons ((operator (eql 'setq)) operands lexenv)
  (compile-setq operands lexenv))

(defstruct (simple-var-lexenv (:include lexenv))
  "A lambda list with no special variables or partial binding.
Suitable for simple lambdas and LET, but not LET* or ll keywords."
;; extend with decl info (eg types) in the future?
  vars)

(defun specialp (symbol env)
  ;; only returns T if the symbol is DEFINITELY special.
  (climbing-lexenv env
    (empty-lexenv
     ;; there's no way to check if a symbol is known special
     ;;  in the host lisp, so we kinda... bullshit it.
     (return (boundp symbol)))
    (lexenv)))

(defun lookup-symbol (symbol lexenv)
  ;; symbol is guaranteed already macroexpanded
  (let ((i 0))
    (climbing-lexenv lexenv
      (empty-lexenv
       ;; total guess :(
       (return 'special))
      (simple-var-lexenv
       (let ((maybe (position symbol (simple-var-lexenv-vars lexenv))))
	 (if maybe
	     (return (values lexenv maybe i))
	     (incf i))))
      (compiler-lexenv) ; don't increment
      (lexenv (incf i)))))

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

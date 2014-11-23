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

(defun lookup-symbol (symbol lexenv)
  ;; symbol is guaranteed already macroexpanded
  (climbing-lexenv lexenv i
    (empty-lexenv
     (unless (boundp symbol)
       ;; there's no way to check if a symbol is known special
       ;; (in the host lisp)
       ;; so we kinda... bullshit it.
       (warn "unbound variable ~a" symbol))
     (return 'special))
    (simple-var-lexenv
     (let ((maybe (position symbol (simple-var-lexenv-vars lexenv))))
       (when maybe
	 (return (values lexenv maybe i)))))))

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

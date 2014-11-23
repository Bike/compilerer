(in-package #:compilerer)

(defmethod compile-cons ((operator (eql 'block)) operands lexenv)
  (compile-block (first operands) (rest operands) lexenv))

(defmethod compile-cons ((operator (eql 'return-from)) operands lexenv)
  (assert (= (length operands) 2))
  (compile-block (second operands) (third operands) lexenv))

(defstruct (block-lexenv (:include lexenv))
  (name :type symbol)
  (ctag :type symbol))

(defun find-block-tag (lexenv name)
  (climbing-lexenv lexenv ignore
    (empty-lexenv (error "Missing block tag ~a" name))
    (block-lexenv
     (when (eql name (block-lexenv-name lexenv))
       (return (block-lexenv-ctag lexenv))))))

(defun compile-block (name forms lexenv)
  (compile-progn forms (make-block-lexenv
			lexenv
			name
			(gensym (concatenate 'string "BLOCK-" name)))))

(defun compile-return-from (name values-form lexenv)
  (let ((ctag (find-block-tag lexenv name))
	(thunk (compile-form values-form lexenv)))
    (lambda (frame)
      (throw ctag (funcall thunk frame)))))

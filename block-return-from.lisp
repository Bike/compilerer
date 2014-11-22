(defstruct (block-lexenv (:include lexenv))
  (name :type symbol)
  (ctag :type symbol))

(defun find-block-tag (lexenv name)
  (loop for lexenv = lexenv then (lexenv-parent lexenv)
     when (empty-lexenv-p lexenv)
     do (error "Missing block tag ~a" name)
     when (and (block-lexenv-p lexenv) (eql name (block-lexenv-name lexenv)))
     return (block-lexenv-ctag lexenv)))

(defun compile-block (name forms lexenv)
  (compile-progn forms (make-block-lexenv
			name
			(gensym (concatenate 'string "BLOCK-" name)))))

(defun compile-return-from (name values-form lexenv)
  (let ((ctag (find-block-tag lexenv name))
	(thunk (compile-form values-form lexenv)))
    (lambda (frame)
      (throw ctag (funcall thunk frame)))))

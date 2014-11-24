(in-package #:compilerer)

(defmethod compile-cons ((operator (eql 'progn)) operands lexenv)
  (compile-progn operands lexenv))

;; useful for e.g. setq
(defun compile-seq (thunks)
  (let* ((len (length thunks))
	 (compiled (make-array len :initial-contents thunks))
	 ;; there are better ways to do this, to say the least
	 lazy)
    (lambda (stack)
      (loop for i from 0 below len
	 do (setf lazy (funcall (aref compiled i) stack))
	 finally (return lazy)))))

(defun compile-progn (forms lexenv)
  (compile-seq (mapcar (lambda (form) (compile-form form lexenv)) forms)))

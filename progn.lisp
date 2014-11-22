;; useful for e.g. setq
(defun compile-seq (thunks)
  (let* ((len (length forms))
	 (compiled (make-array len :element-type 'closure-compiled
			       :initial-contents thunks))
	 ;; there are better ways to do this, to say the least
	 lazy)
    (lambda (frame)
      (loop for i from 0 do (setf lazy (funcall (aref compiled i) frame))
	 finally (return lazy)))))

(defun compile-progn (forms lexenv)
  (compile-seq (mapcar (lambda (form) (compile-form form lexenv)) forms)))

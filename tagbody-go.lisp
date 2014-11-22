;; imaginary closure compiler
;; interface is hopefully intuitive, e.g. (compile-<thing> stuff lexenv)

(deftype closure-compiled () '(function (vector) *))

(defun parse-tagbody (statements)
  (let ((parsed (list nil)))
    (dolist (stmt statements)
      (if (or (symbolp stmt) (integerp stmt)) ; glossary "go tag"
	  (progn (push (list 'go stmt) (first parsed))
		 (push (list stmt) parsed))
	  (push stmt (first parsed))))
    (nreverse (mapcar #'nreverse parsed))))

;; example: (parse-tagbody '((print 'out-of-loop) :infty (print 'loop) (go :infty)))
#||
(((PRINT 'OUT-OF-LOOP) (:GO INFTY))
 ((:INFTY (PRINT 'LOOP) (GO :INFTY)))
||#

;; tagbody-lexenv can be a struct with three fields, one of which is the parent
;; accessors etc. are named in the normal way

(defun compile-tagbody (statements lexenv)
  (let* ((parsed (parse-tagbody statements))
	 (ctag (gensym "TAGBODY"))
	 (tags (loop for i from 0 for body in (rest parsed)
		  collect (cons (first body) i)))
	 (env (make-tagbody-lexenv ctag tags lexenv)))
    (let ((main (compile-progn (first parsed) env))
	  (destinations (make-array (length tags) :element-type 'closure-compiled)))
      (loop for i below (length tags)
	 for body in (rest parsed)
	 do (setf (aref destinations i)
		  (compile-progn (rest body) env)))
      (lambda (frame)
	(block normal-return
	  (let ((dest main))
	    (loop
	       ;; the aref can't fail since we're making sure everything's cool
	       ;; at compile time
	       (setf dest (aref destinations
				(catch ctag
				  (return-from normal-return (funcall dest frame)))))))
	  ;; tagbody can't return anything but nil
	  nil)))))

(defun compile-go (tag lexenv)
  ;; hopefully this would be factored out into a function,
  ;; but i'll write out a loop to make things clear, hopefully.
  (multiple-value-bind (env id)
      (loop for e = lexenv then (lexenv-parent lexenv)
	 when (empty-environment-p e)
	 do (error "Attempted to GO to nonexistent tag ~a" tag)
	 when (tagbody-lexenv-p e)
	 ;; "Tags are compared with eql", which i guess should be "as by"
	 do (let ((maybe (assoc tag (tagbody-lexenv-tags e))))
	      (when maybe
		(return (values e (cdr maybe))))))
    (let ((ctag (tagbody-lexenv-ctag env)))
      (lambda (frame)
	(throw ctag id)))))

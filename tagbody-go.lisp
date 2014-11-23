(in-package #:compilerer)

(defmethod compile-cons ((operator (eql 'tagbody)) operands lexenv)
  (compile-tagbody operands lexenv))

(defmethod compile-cons ((operator (eql 'go)) operands lexenv)
  (assert (= (length operands) 1))
  (compile-go (first operands) lexenv))

(defstruct (tagbody-lexenv (:include lexenv))
  tags
  ctag)

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
 (:INFTY (PRINT 'LOOP) (GO :INFTY)))
||#

;;; as for the actual operation. We have one thunk we're at,
;;; "dest", which is main to begin with.
;;; We call it. Three possibilities:
;;; 1) it just ends. we return-from out and return nil.
;;; 2) control is transferred past the tagbody
;;;    (e.g. to a higher tagbody or block).
;;;    in this case we don't need to do anything more.
;;; 3) it GOes to this tagbody. GOing means THROWing
;;;    an integer ID to this (catch ctag ...).
;;;    the integer ID is an index of a thunk.
;;;    so, we set this new thunk to be "dest", and repeat.
;;; This should work with weird situations like GOs from closures.

(defun compile-tagbody (statements lexenv)
  (let* ((parsed (parse-tagbody statements))
	 (ctag (gensym "TAGBODY"))
	 (tags ; an alist: tag -> integer
	  (loop for i from 0 for body in (rest parsed)
	     collect (cons (first body) i)))
	 (env (make-tagbody-lexenv ctag tags lexenv))
	 (main (compile-progn (first parsed) env))
	 (destinations (make-array (length tags))))
    (map-into destinations (lambda (b) (compile-progn (rest b) env))
	      (rest parsed))
    (lambda (frame)
      (block normal-return
	(loop
	   ;; this is a "clever" replacement for (loop (setf dest ...))
	   for dest = main then
	     (aref destinations
		   (catch ctag
		     (return-from normal-return (funcall dest frame)))))))
    ;; tagbody can't return anything but nil
    nil))

(defun compile-go (tag lexenv)
  (climbing-lexenv lexenv ignore
    (empty-lexenv (error "Attempted to GO to nonexistent tag ~a" tag))
    (tagbody-lexenv
     ;; "Tags are compared with eql", which i guess should be "as by"
     (let ((maybe (assoc tag (tagbody-lexenv-tags lexenv))))
       (when maybe
	 (return ;; just hope host factors out unused variables
	   (let ((ctag (tagbody-lexenv-ctag lexenv))
		 (id (cdr maybe)))
	     (lambda (frame) (throw ctag id)))))))))

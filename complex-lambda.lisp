(in-package #:compilerer)

;;; now the hard bits

(defstruct (complex-var-lexenv (:include lexenv))
  (lvars (initarg-not-provided) :type simple-vector))

(defstruct (partial-lexenv (:include lexenv))
  lvars)

(defun in-frame (frame pos) (svref frame pos))
(defun (setf in-frame) (v frame pos) (setf (svref frame pos) v))

(defun frame-pos (renv name) (svref renv name))

(defun make-partial (lexenv var)
  (declare (complex-var-lexenv lexenv))
  (make-partial-lexenv
   :parent lexenv
   :lvars (make-array
	   (position var (complex-var-lexenv-lvars lexenv))
	   :displaced-to (complex-var-lexenv-lvars lexenv))))

(defun keyword-parser (pspecs aok-p lexenv)
  (flet ((spec-key (s) (first s))
	 (spec-var (s) (second s))
	 (spec-init (s) (third s))
	 (spec-suppliedp (s) (fourth s)))
    (let ((positions (make-array (length pspecs)))
	  (specs (make-array (length pspecs)))
	  ;; "runtime environment", maps symbols to frame pos
	  (renv (complex-var-lexenv-lvars lexenv)))
      ;; convert from p-o-l-l format
      (loop for spec in pspecs
	 for i from 0
	 do (setf (svref specs i)
		  (list (caar spec) ; keyword-name
			(cadar spec) ; name
			(compile-form (second spec) ; init
				      (make-partial
				       lexenv (second spec)))
			(third spec) #|suppliedp|#)))
      (map-into positions #'spec-key specs)
      (lambda (stack plist)
	(let ((bound (make-array (length specs)))
	      (seen-aok aok-p) ; &aok overrides :aok
	      (aok aok-p)
	      (unknown-keys nil))
	  ;; First set all the parameters from the call.
	  (doplist (key val plist)
	    ;; you can do &key allow-other-keys if you really hate us
	    ;; so after this check keep going normally
	    (when (and (eq key :allow-other-keys) (not seen-aok))
	      (setf seen-aok t aok val))
	    (let ((pos (position key positions)))
	      (cond (pos ; keyword is known for this fn
		     (unless (svref bound pos) ; preserve ltr
		       (let* ((spec (svref specs pos))
			      (suppliedp (spec-suppliedp spec)))
			 (setf (svref bound pos) t)
			 (when suppliedp
			   (setf (in-frame (car stack)
					   (frame-pos renv suppliedp))
				 t))
			 (setf (in-frame
				(car stack)
				(frame-pos renv (spec-var spec)))
			       val))))
		    ((not (eq key :allow-other-keys))
		     ;; aok is always ok
		     (push key unknown-keys)))))
	  ;; Next, plug in defaults.
	  (loop for spec across specs ; ltr
	     for boundp across bound
	     do (unless boundp
		  (let ((val (funcall (spec-init spec) stack)))
		    (setf (in-frame (car stack)
				    (frame-pos renv (spec-var spec)))
			  val)
		    (when (and (not seen-aok)
			       (eq (spec-key spec) :allow-other-keys))
		      (setf aok val)))))
	  ;; Finally (finally!) warn on extra keywords.
	  (when (and unknown-keys (not aok))
	    (warn "Unknown keywords ~{~s~^, ~}." unknown-keys)))
	(values)))))

(defun all-vars (req opt rest key aux)
  (coerce (append req
		  (loop for o in opt collect (first o)
		     when (third o) collect it)
		  (when rest (list rest))
		  (loop for k in key collect (cadar k)
		     when (third k) collect it)
		  (mapcar #'first aux))
	  '(simple-vector symbol)))

(defun compile-complex-lambda (req opt rest key aok-p aux key-p
			       body lexenv)
  (let* ((lvars (all-vars req opt rest key aux))
	 (env (make-complex-var-lexenv :parent lexenv :lvars lvars))
	 (lreq (length req))
	 (lkey (length key))
	 (restpos (if rest (position rest lvars) nil))
	 ;; only build a parser if we need to
	 (keyparse (if key-p (keyword-parser key aok-p env) nil))
	 (auxinits (mapcar (lambda (s)
			     (compile-form
			      (second s)
			      (make-partial env (first s)))) aux))
	 (thunk (compile-progn body env)))
    (lambda (stack)
      (lambda (&rest args)
	(let ((nbound 0))
	  ;; required
	  (loop for i below lreq for (a . rest) in args do
	       (setf (in-frame (first stack) i) a)
	     finally (setf args rest))
	  (incf nbound lreq)
	  ;; optional
	  ;; you know what fuck these ok
	  ;; rest
	  (when rest
	    (setf (in-frame (first stack) restpos) args)
	    (incf nbound 1))
	  ;; key
	  (when key-p (funcall keyparse stack args))
	  (incf nbound lkey)
	  ;; aux
	  (when aux
	    (loop for i from nbound for init in auxinits do
		 (setf (in-frame (first stack) i)
		       (funcall init stack))))
	  ;; Time to go
	  (funcall thunk stack))))))

(in-package #:compilerer)

;;; CL is less than friendly to defining your own environments.
;;; We define our own macro-function and compiler-macro-function
;;;  that override the built in ones and use our lexenvs, or fall
;;;  back to host lexenvs otherwise.
;;; User code being compiled by us must use our macro-function etc.
;;;  rather than those of the CL package. Bad.
;;; All code in compilerer should use our macroexpand etc., i.e. w/o
;;;  package qualifier, except in this file.

(defstruct (macro-lexenv (:include compiler-lexenv))
  ;; just an alist: name -> function
  macros) 

(defstruct (symbol-macro-lexenv (:include compiler-lexenv))
  ;; an alist symbol -> expansion
  macros)

(defun %macro-function (name env)
  (climbing-lexenv env
    (empty-lexenv (cl:macro-function name))
    (macro-lexenv
     (let ((a (assoc name (macro-lexenv-macros env) :test #'equal)))
       ;; #'equal for (setf ...)
       (when a (return (cdr a)))))
    (lexenv)))

(defun %compiler-macro-function (name env)
  ;; Having our own compiler-macro-function isn't really necessary,
  ;;  but later I might add lexical compiler macros, who knows?
  (declare (ignore env))
  (cl:compiler-macro-function name))

(defun symbol-macro-expansion (symbol env)
  (if (typep env 'lexenv-designator)
      (climbing-lexenv env
	(empty-lexenv (return (cl:macroexpand-1 symbol)))
	(symbol-macro-lexenv
	 (let ((a (assoc symbol (symbol-macro-lexenv-macros env))))
	   (when a (return (values (cdr a) t)))))
	(lexenv))
      ;; native env, maybe
      (cl:macroexpand-1 symbol env)))

;; grabbed from sbcl
(defun macroexpand-1 (form &optional env)
  (cond ((and (consp form) (symbolp (car form)))
	 (let ((def (macro-function (car form) env)))
	   (if def
	       (values (funcall *macroexpand-hook*
				def form env)
		       t)
	       (values form nil))))
	((symbolp form)
	 (flet ((perform-symbol-expansion (symbol expansion)
                  ;; CLHS 3.1.2.1.1 specifies that symbol-macros are
                  ;;  expanded via the macroexpand hook, too.
                  (funcall *macroexpand-hook*
                           (lambda (form env)
			     (declare (ignore form env))
			     expansion)
                           symbol
                           env)))
	   (multiple-value-bind (exp exists?)
	       ;; have to be weird since we can define symbols
	       ;;  to expand to NIL or anything
	       (symbol-macro-expansion form env)
	     (if exists?
		 (values (perform-symbol-expansion form exp) t)
		 (values form nil)))))
	(t (values form nil))))

(defun compiler-macroexpand-1 (form &optional env)
  (if (and (consp form) (symbolp (car form)))
      (let ((def (compiler-macro-function (car form) env)))
	(if def
	    (let ((new (funcall *macroexpand-hook* def form env)))
	      (if (eq new form) ; cmf declined expansion
		  (values form nil)
		  (values new t)))
	    (values form nil)))
      (values form nil)))

(macrolet ((def (f me)
	     (let ((cl-f (find-symbol (symbol-name f) "CL"))
		   (%f (intern (concatenate 'string
					    "%" (symbol-name f))))
		   (me-1 (intern (concatenate 'string
					      (symbol-name me)
					      "-1"))))
	       `(progn
		  (defun ,f (name &optional env)
		    (typecase env
		      (null (,cl-f name))
		      (lexenv (,%f name env))
		      (t (,cl-f name env))))
		  (defun ,me (form &optional env)
		    (labels ((frob (form expanded)
			       (multiple-value-bind
				     (new-form newly-expanded-p)
				   (,me-1 form env)
				 (if newly-expanded-p
				     (frob new-form t)
				     (values new-form expanded)))))
		      (frob form nil)))))))
  (def macro-function macroexpand)
  ;; compiler macroexpand is not standard-defined,
  ;;  but should come in handy.
  (def compiler-macro-function compiler-macroexpand))

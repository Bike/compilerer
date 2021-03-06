(in-package #:compilerer-test)

(def-suite :compilerer)

(in-suite :compilerer)

(defun in-agreement (form)
  (let (o1 s1 o2 s2)
    (setf s1 (with-output-to-string (*standard-output*)
	       (setf o1 (multiple-value-list (eval form)))))
    (setf s2 (with-output-to-string (*standard-output*)
	       (setf o2 (multiple-value-list
			 (exec* (compile-form* form))))))
    (and (equal o1 o2) (equal s1 s2))))

(defmacro agrees (form)
  `(is (in-agreement ',form)))

(test constants
  (agrees 4))

(test specials
  (agrees *gensym-counter*)
  (let ((thunk (compile-form* '*gensym-counter*))
	(*gensym-counter* (1+ *gensym-counter*)))
    (is (eql (exec* thunk) *gensym-counter*))))

(test quote
  (agrees (quote foo))
  (agrees (quote (a b))))

(test function-name
  (agrees #'documentation)
  (agrees #'(setf documentation)))

(test call-global
  (agrees (+ 1 1))
  (agrees (print 'test)))

(test macro-global
  (agrees (cond ((= 1 1) nil))))

(test progn
  (agrees (progn (print 'foo) (+ 18 19))))

(test lambda-simple
  (is-true (funcall (exec* (compile-form* '#'(lambda () t)))))
  (let ((x (random 10)))
    (is (= x (funcall (exec* (compile-form* '#'(lambda (x) x))) x)))
    (is (= x (exec* (compile-form* `(funcall #'(lambda (x) x) ,x)))))))

(test lambda-forms
  (agrees ((lambda (x) x) 9)))

(test multiple-values
  (agrees (multiple-value-call #'list
	    (values 2 3) 9 (values 4) (values) (values 6))))

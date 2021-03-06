(in-package #:compilerer)

(defmethod compile-cons ((operator (eql 'catch)) operands lexenv)
  (compile-catch (first operands) (rest operands) lexenv))

(defmethod compile-cons ((operator (eql 'throw)) operands lexenv)
  (assert (= (length operands) 2))
  (compile-throw (first operands) (second operands) lexenv))

(defun compile-catch (tag body lexenv)
  (let ((tag (compile-form tag lexenv))
	(body (compile-form body lexenv)))
    (lambda (stack)
      (catch (funcall tag stack) (funcall body stack)))))

(defun compile-throw (tag result lexenv)
  (let ((tag (compile-form tag lexenv))
	(res (compile-form result lexenv)))
    (lambda (stack)
      (throw (funcall tag stack) (funcall res stack)))))

(in-package #:compilerer)

(defgeneric compile-form (form lexenv)
  (:method :around (form lexenv)
    (call-next-method (macroexpand form lexenv) lexenv))
  (:method ((form cons) lexenv)
    (compile-cons (first form) (rest form) lexenv)))

(defgeneric compile-cons (operator operands lexenv))

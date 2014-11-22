(defun compile-constant (constant)
  (lambda (frame) (declare (ignore frame)) constant))

(defun compile-quote (thing lexenv)
  (declare (ignore lexenv))
  (compile-constant thing))

(defstruct lexenv
  (parent :type lexenv))

(defstruct (empty-lexenv (:include lexenv)))

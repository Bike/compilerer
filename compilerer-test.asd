(asdf:defsystem #:compilerer-test
  :depends-on (#:compilerer #:fiveam)
  :version "0.1"
  :components ((:module "test"
		 :components ((:file "package")
			      (:file "basic" :depends-on ("package")))))
  ; warning: cargo cult (from bordeaux-threads-test)
  :in-order-to ((asdf:test-op (asdf:load-op compilerer-test)))
  :perform (asdf:test-op :after (op c)
	     (funcall (find-symbol "RUN!" "5AM") :compilerer)))

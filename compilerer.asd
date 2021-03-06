(asdf:defsystem #:compilerer
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "interface" :depends-on ("package" "form"))
	       (:file "lexenv" :depends-on ("package"))
	       (:file "macros" :depends-on ("lexenv" "package"))
	       (:file "form" :depends-on ("macros" "package"))
	       (:file "constant" :depends-on ("form" "package"))
	       (:file "call" :depends-on ("function" "form" "package"))
	       (:file "progn" :depends-on ("form" "package"))
	       (:file "function" :depends-on ("lambda" "package"))
	       (:file "lambda"
		      :depends-on ("progn"
				   "setq-variable" ; for lookup-symbol
				   "lexenv" "package"))
	       (:file "catch-throw" :depends-on ("form" "package"))
	       (:file "if" :depends-on ("form" "package"))
	       (:file "setq-variable"
		      :depends-on ("form" "lexenv" "package"))
	       (:file "tagbody-go"
		      :depends-on ("form" "lexenv" "package"))
	       (:file "block-return-from"
		      :depends-on ("form" "lexenv" "package")))
  :in-order-to ((asdf:test-op (asdf:load-op compilerer-test)))
  :perform (asdf:test-op :after (op c) (asdf:test-system :compilerer-test)))

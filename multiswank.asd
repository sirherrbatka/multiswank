(asdf:defsystem #:multiswank
  :depends-on (#:swank-client #:alexandria)
  :serial t
  :pathname "source"
  :components ((:file "code")))

(asdf:defsystem #:multiswank
  :depends-on (#:swank-client #:alexandria #:serapeum)
  :serial t
  :pathname "source"
  :components ((:file "code")))

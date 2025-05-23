(asdf:defsystem "vm1"
  :description "A small register based virtual machine implemented in Common Lisp"
  :author "Mattias Brändström"
  :license "MIT"
  :depends-on ("fiveam")
  :serial t
  :components ((:file "package")
               (:file "vm-error")
               (:file "vm-value")
               (:file "frame")
               (:file "vm")
               (:file "assembler")
               (:file "tests/frame-test")))

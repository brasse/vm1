(asdf:defsystem "vm1"
  :description "A small register based virtual machine implemented in Common Lisp"
  :author "Mattias Brändström"
  :license "MIT"
  :depends-on ("fiveam" "split-sequence")
  :serial t
  :components ((:file "package")
               (:file "vm-error")
               (:file "vm-value")
               (:file "frame")
               (:file "vm")
               (:file "assembler")
               (:file "compiler")
               (:file "examples/examples-asm")
               (:file "tests/util")
               (:file "tests/frame-test")
               (:file "tests/instruction-test")
               (:file "tests/vm-value-test")
               (:file "tests/examples-test")))

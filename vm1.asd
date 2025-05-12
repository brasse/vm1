(asdf:defsystem "vm1"
  :description "A small register based virtual machine implemented in Common Lisp"
  :author "Mattias Brändström"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "vm-error")
               (:file "vm")))

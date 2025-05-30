(in-package :vm1)

(def-suite instruction-test)

(in-suite instruction-test)

(defun setup (program)
  (make-vm-state
   :program program
   :pc 0
   :frame-stack (list (make-frame))))

(test int-literal
  (let ((vm (setup #((const foo 123)))))
    (run-program vm)
    (let ((value (frame-get-reg (car (vm-state-frame-stack vm)) 'foo)))
      (is (eq :int (vm-value-type value)))
      (is (= 123 (vm-value-payload value))))))

(test string-literal
  (let ((vm (setup #((const foo "foo")))))
    (run-program vm)
    (let ((value (frame-get-reg (car (vm-state-frame-stack vm)) 'foo)))
      (is (eq :string (vm-value-type value)))
      (is (string= "foo" (vm-value-payload value))))))

(test bool-literal
  (let ((vm (setup #((const foo false)))))
    (run-program vm)
    (let ((value (frame-get-reg (car (vm-state-frame-stack vm)) 'foo)))
      (is (eq :bool (vm-value-type value)))
      (is (eq nil (vm-value-payload value)))))

  (let ((vm (setup #((const foo true)))))
    (run-program vm)
    (let ((value (frame-get-reg (car (vm-state-frame-stack vm)) 'foo)))
      (is (eq :bool (vm-value-type value)))
      (is (eq t (vm-value-payload value))))))

(test mov
  (let ((vm (setup #((const foo 123) (mov bar foo)))))
    (run-program vm)
    (let ((value (frame-get-reg (car (vm-state-frame-stack vm)) 'bar)))
      (is (eq :int (vm-value-type value)))
      (is (= 123 (vm-value-payload value))))))

(test add-literals
  (let ((vm (setup #((add foo 100 23)))))
    (run-program vm)
    (let ((value (frame-get-reg (car (vm-state-frame-stack vm)) 'foo)))
      (is (eq :int (vm-value-type value)))
      (is (= 123 (vm-value-payload value))))))

(test add-regs
  (let ((vm (setup #((const foo 100) (const bar 50) (add baz foo bar)))))
    (run-program vm)
    (let ((value (frame-get-reg (car (vm-state-frame-stack vm)) 'baz)))
      (is (eq :int (vm-value-type value)))
      (is (= 150 (vm-value-payload value))))))

(test add-fails-on-not-int
  (let ((vm (setup #((add foo 100 "foo")))))
    (signals vm-type-error (run-program) vm)))

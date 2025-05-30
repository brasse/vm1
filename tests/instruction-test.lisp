(in-package :vm1)

(def-suite instruction-test)

(in-suite instruction-test)

(defun setup (program)
  (make-vm-state
   :program program
   :pc 0
   :frame-stack (list (make-frame))
   :string-table (make-hash-table)))

(defmacro single-reg-test (name reg program &body body)
  `(test ,name
     (let ((vm (setup ,program)))
       (run-program vm)
       (let ((value (frame-get-reg (car (vm-state-frame-stack vm)) ,reg)))
         ,@body))))

(single-reg-test
    int-literal 'foo #((const foo 123))
  (is (eq :int (vm-value-type value)))
  (is (= 123 (vm-value-payload value))))


(single-reg-test
    string-literal 'foo #((const foo "foo"))
  (is (eq :string (vm-value-type value)))
  (is (string= "foo" (vm-value-payload value))))

(single-reg-test
    bool-literal-false 'foo #((const foo false))
  (is (eq :bool (vm-value-type value)))
  (is (eq nil (vm-value-payload value))))

(single-reg-test
    bool-literal-true 'foo #((const foo true))
  (is (eq :bool (vm-value-type value)))
  (is (eq t (vm-value-payload value))))

(single-reg-test
    mov-moves 'foo #((const foo 123) (mov bar foo))
  (is (eq :int (vm-value-type value)))
  (is (= 123 (vm-value-payload value))))

(single-reg-test
    add-literals 'foo #((add foo 100 23))
  (is (eq :int (vm-value-type value)))
  (is (= 123 (vm-value-payload value))))

(single-reg-test
    add-regs 'baz #((const foo 100) (const bar 50) (add baz foo bar))
  (is (eq :int (vm-value-type value)))
  (is (= 150 (vm-value-payload value))))

(test add-fails-on-not-int
  (let ((vm (setup #((add foo 100 "foo")))))
    (signals vm-type-error (run-program vm))))

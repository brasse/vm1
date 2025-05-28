(in-package :vm1)

(def-suite instruction-test)

(in-suite instruction-test)

(test int-literal
  (let ((frame-stack (list (make-frame))))
    (execute '(const foo 123) frame-stack 0)
    (let ((value (frame-get-reg (car frame-stack) 'foo)))
      (is (eq :int (vm-value-type value)))
      (is (= 123 (vm-value-payload value))))))

(test string-literal
  (let ((frame-stack (list (make-frame))))
    (execute '(const foo "foo") frame-stack 0)
    (let ((value (frame-get-reg (car frame-stack) 'foo)))
      (is (eq :string (vm-value-type value)))
      (is (string= "foo" (vm-value-payload value))))))

(test bool-literal
  (let ((frame-stack (list (make-frame))))
    (execute '(const foo false) frame-stack 0)
    (let ((value (frame-get-reg (car frame-stack) 'foo)))
      (is (eq :bool (vm-value-type value)))
      (is (eq nil (vm-value-payload value)))))

  (let ((frame-stack (list (make-frame))))
    (execute '(const foo true) frame-stack 0)
    (let ((value (frame-get-reg (car frame-stack) 'foo)))
      (is (eq :bool (vm-value-type value)))
      (is (eq t (vm-value-payload value))))))

(test mov
  (let ((frame-stack (list (make-frame))))
    (execute '(const foo 123) frame-stack 0)
    (execute '(mov bar foo) frame-stack 0)
    (let ((value (frame-get-reg (car frame-stack) 'bar)))
      (is (eq :int (vm-value-type value)))
      (is (= 123 (vm-value-payload value))))))

(test add-literals
  (let ((frame-stack (list (make-frame))))
    (execute '(add foo 100 23) frame-stack 0)
    (let ((value (frame-get-reg (car frame-stack) 'foo)))
      (is (eq :int (vm-value-type value)))
      (is (= 123 (vm-value-payload value))))))

(test add-regs
  (let ((frame-stack (list (make-frame))))
    (execute '(const foo 100) frame-stack 0)
    (execute '(const bar 50) frame-stack 0)
    (execute '(add baz foo bar) frame-stack 0)
    (let ((value (frame-get-reg (car frame-stack) 'baz)))
      (is (eq :int (vm-value-type value)))
      (is (= 150 (vm-value-payload value))))))

;; (test add-fail-on-not-int
;;   (let ((frame-stack (list (make-frame))))
;;     (execute '(add foo 100 "foo") frame-stack 0)
;;     (let ((value (frame-get-reg (car frame-stack) 'foo)))
;;       (is (eq :int (vm-value-type value)))
;;       (is (= 123 (vm-value-payload value))))))

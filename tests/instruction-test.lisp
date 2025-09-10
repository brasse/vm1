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

;; add str tests

(test vec-instruction-happy-path
  (let ((prog #((vec-make v 3)          ; v = [none none none]
                (vec-set v 0 42)
                (vec-get x v 0)
                (print x)               ; prints 42
                (vec-len n v)
                (print n))))            ; prints 3
    (is (equalp
         '("42" "3")
         (capture-output
          (lambda () (fancy-run-program :program prog)))))))

(test vec-set-type-error
  (let* ((vm (setup #((vec-set 123 0 0))))) ; first arg not a vector
    (signals vm-type-error (run-program vm))))

;;;;
;;;;  map instruction tests
;;;;

(single-reg-test
    map-make-produces-map 'm #((map-make m))
  (is (eq :map (vm-value-type value))))

(test map-set-get-happy-path
  (let ((prog #((map-make m)
                (const k "foo")
                (const v 42)
                (map-set m k v)
                (map-get x m k)
                (print x))))
    (is (equal
         '("42")
         (capture-output
          (lambda () (fancy-run-program :program prog)))))))

(test map-has-true-and-false
  (let ((prog #((map-make m)
                (const k "k")
                (map-set m k 1)
                (map-has present m k)      ; true
                (map-has absent  m "z")    ; false
                (print present)
                (print absent))))
    (is (equal
         '("true" "false")
         (capture-output
          (lambda () (fancy-run-program :program prog)))))))

(test map-set-type-error-non-map
  (let ((vm (setup #((map-set 123 "foo" 0)))))
    (signals vm-type-error (run-program vm))))


(test map-get-type-error-non-map
  (let ((vm (setup #((map-get x 123 "foo")))))
    (signals vm-type-error (run-program vm))))

(single-reg-test
    to-str-int 'result #((const val 42) (to-str result val))
  (is (eq :string (vm-value-type value)))
  (is (string= "42" (vm-value-payload value))))

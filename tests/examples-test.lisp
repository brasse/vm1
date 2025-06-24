(in-package :vm1)

(def-suite examples)

(in-suite examples)

(test factorial-asm-itertive-works
  (is (equalp
       '("40320")
       (capture-output
        (lambda () (fancy-run-program :program +factorial-asm-iterative+))))))

(test factorial-asm-recursive-works
  (is (equalp
       '("40320")
       (capture-output
        (lambda () (fancy-run-program :program +factorial-asm-recursive+))))))

(test factorial-asm-tail-recursive-works
  (is (equalp
       '("40320")
       (capture-output
        (lambda () (fancy-run-program :program +factorial-asm-tail-recursive+))))))

(test fibonacci-asm-iterative-works
  (is (equalp
       '("6765")
       (capture-output
        (lambda () (fancy-run-program :program +fibonacci-asm-iterative+))))))

(test fibonacci-asm-recursive-works
  (is (equalp
       '("6765")
       (capture-output
        (lambda () (fancy-run-program :program +fibonacci-asm-recurursive+))))))

(test fibonacci-asm-tail-recursive-works
  (is (equalp
       '("6765")
       (capture-output
        (lambda () (fancy-run-program :program +fibonacci-asm-tail-recurursive+))))))

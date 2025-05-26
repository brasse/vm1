(in-package :vm1)

(def-suite frame-test)

(in-suite frame-test)

(test frame-add-scope-add-scope
  (let ((frame (make-frame)))
    (frame-push-scope frame)
    (frame-push-scope frame)
    (is (= 3 (length (frame-scopes frame))))))

(test frame-pop-scope-removes-scope
  (let ((frame (make-frame)))
    (frame-push-scope frame)
    (frame-push-scope frame)
    (frame-pop-scope frame)
    (is (= 2 (length (frame-scopes frame))))))

(test frame-get-and-set-reg-gets-and-sets
  (let ((frame (make-frame)))
    (frame-push-scope frame)
    (frame-set-reg frame 'foo 1)
    (frame-set-reg frame 'bar 2)
    (frame-push-scope frame)
    (frame-set-reg frame 'bar 20)
    (is (= 1 (frame-get-reg frame 'foo)))
    (is (= 20 (frame-get-reg frame 'bar)))))

(test frame-get-arg-returns-arg
  (let ((frame (make-frame :args '(10 20))))
    (is (= 10 (frame-get-arg frame 0)))
    (is (= 20 (frame-get-arg frame 1)))))

(test frame-stack-push-and-pop-pushs-and-pops
  (let ((stack0 (list (make-frame))))
    (frame-set-reg (car stack0) 'foo 1)
    (let ((stack1 (frame-stack-push-frame stack0 nil 0)))
      (frame-set-reg (car stack1) 'foo 10)
      (is (= 10 (frame-get-reg (car stack1) 'foo)))
      (let ((stack2 (frame-stack-pop-frame stack1)))
        (is (= 1 (frame-get-reg (car stack2) 'foo)))))))

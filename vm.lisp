(in-package :vm1)

(defun current-instruction ()
  (declare (special *instruction*))
  *instruction*)

(defun resolve-value (frame-stack x)
  (if (symbolp x)
      (frame-stack-get-reg frame-stack x)
      (vm-value-make-literal x)))

(defmacro def-int-binary-op (name op)
  `(defun ,name (v1 v2)
     (let ((t1 (vm-value-type v1)) (t2 (vm-value-type v2))
           (v1 (vm-value-payload v1)) (v2 (vm-value-payload v2)))
       (unless (and (eq t1 :int) (eq t2 :int))
         (error 'vm-type-error :instruction (current-instruction)
                               :expected :int  :actual (list t1 t2)))
       (vm-value-make-int (funcall ,op v1 v2)))))

(def-int-binary-op add (lambda (a b) (+ a b)))
(def-int-binary-op sub (lambda (a b) (- a b)))
(def-int-binary-op mul (lambda (a b) (* a b)))
(def-int-binary-op div
    (lambda (a b) (if (zerop b)
                      (error 'vm-divide-by-zero :instruction (current-instruction))
                      (truncate a b))))
(def-int-binary-op vm1-mod
    (lambda (a b) (if (zerop b)
                      (error 'vm-divide-by-zero :instruction (current-instruction))
                      (nth-value 1 (truncate a b)))))
(def-int-binary-op vm1-eq (lambda (a b) (if (= a b) 1 0)))
(def-int-binary-op gt (lambda (a b) (if (> a b) 1 0)))
(def-int-binary-op lt (lambda (a b) (if (< a b) 1 0)))

(defun jmp (target)
  (cond
    ((not (integerp target))
     (error 'vm-error :instruction (current-instruction)
                      :message (format nil "malformed target: ~A" target)))
    (t `(:jump ,target))))

(defun jz (condition target)
  (cond
    ((not (integerp target))
     (error 'vm-error :instruction (current-instruction)
                      :message (format nil "malformed target: ~A" target)))
    ((vm-value-falsep condition) `(:jump ,target))
    (t '(:continue))))

(defun jnz (condition target)
  (cond
    ((not (integerp target))
     (error 'vm-error :instruction (current-instruction)
                      :message (format nil "malformed target: ~A" target)))
    ((not (vm-value-falsep condition)) `(:jump ,target))
    (t '(:continue))))

(defun call (frame-stack target args return-address)
  (if (integerp target)
      `(:call ,target ,(frame-stack-push-frame frame-stack args return-address))
      (error 'vm-error :instruction (current-instruction)
                       :message (format nil "malformed target: ~A" target))))

(defun ret (frame-stack return-address return-values)
  (let ((return-stack (frame-stack-pop-frame frame-stack)))
    (frame-stack-set-return-values return-stack return-values)
    `(:return ,return-address ,return-stack)))

(defmacro args-1 (arg1 &body body)
  `(let ((,arg1 (cadr instruction)))
     ,@body))

(defmacro args-2 (arg1 arg2 &body body)
  `(let ((,arg1 (cadr instruction)) (,arg2 (caddr instruction)))
     ,@body))

(defmacro args-3 (arg1 arg2 arg3 &body body)
  `(let ((,arg1 (cadr instruction)) (,arg2 (caddr instruction)) (,arg3 (cadddr instruction)))
     ,@body))

(defun execute (instruction frame-stack return-address)
  (let ((head (car instruction)) (*instruction* instruction))
    (declare (special *instruction*))
    (handler-case
        (case head
          (const (args-2 dst literal
                   (frame-stack-set-reg frame-stack dst (vm-value-make-literal literal))
                   '(:continue)))
          (mov (args-2 dst a
                 (frame-stack-set-reg frame-stack dst (resolve-value frame-stack a))
                 '(:continue)))
          (add (args-3 dst a b
                 (frame-stack-set-reg
                  frame-stack
                  dst
                  (add (resolve-value frame-stack a) (resolve-value frame-stack b)))
                 '(:continue)))
          (sub (args-3 dst a b
                 (frame-stack-set-reg
                  frame-stack
                  dst
                  (sub (resolve-value frame-stack a) (resolve-value frame-stack b)))
                 '(:continue)))
          (mul (args-3 dst a b
                 (frame-stack-set-reg
                  frame-stack
                  dst
                  (mul (resolve-value frame-stack a) (resolve-value frame-stack b)))
                 '(:continue)))
          (div (args-3 dst a b
                 (frame-stack-set-reg
                  frame-stack
                  dst
                  (div (resolve-value frame-stack a) (resolve-value frame-stack b)))
                 '(:continue)))
          (mod (args-3 dst a b
                 (frame-stack-set-reg
                  frame-stack
                  dst
                  (vm1-mod (resolve-value frame-stack a) (resolve-value frame-stack b)))
                 '(:continue)))
          (eq (args-3 dst a b
                (frame-stack-set-reg
                 frame-stack
                 dst
                 (vm1-eq (resolve-value frame-stack a) (resolve-value frame-stack b)))
                '(:continue)))
          (gt (args-3 dst a b
                (frame-stack-set-reg
                 frame-stack
                 dst
                 (gt (resolve-value frame-stack a) (resolve-value frame-stack b)))
                '(:continue)))
          (lt (args-3 dst a b
                (frame-stack-set-reg
                 frame-stack
                 dst
                 (lt (resolve-value frame-stack a) (resolve-value frame-stack b)))
                '(:continue)))
          (jmp (args-1 target (jmp target)))
          (jz (args-2 condition target (jz (resolve-value frame-stack condition) target)))
          (jnz (args-2 condition target (jnz (resolve-value frame-stack condition) target)))
          (print (args-1 a
                   (format t "~A~%" (vm-value-str (resolve-value frame-stack a)))
                   '(:continue)))
          (call (destructuring-bind (target . args) (cdr instruction)
                  (let ((resolved-values
                          (loop for arg in args collect (resolve-value frame-stack arg))))
                    (call frame-stack target resolved-values return-address))))
          (ret (let ((resolved-returns
                       (loop for ret in (cdr instruction) collect (resolve-value frame-stack ret))))
                 (ret frame-stack (frame-stack-get-return-address frame-stack) resolved-returns)))
          (get-arg (args-2 dst n
                     (frame-stack-set-reg
                      frame-stack
                      dst
                      (frame-stack-get-arg frame-stack n))
                     '(:continue)))
          (get-ret (args-2 dst n
                     (frame-stack-set-reg
                      frame-stack
                      dst
                      (frame-stack-get-return-value frame-stack n))
                     '(:continue)))
          (get-ret-count (args-1 dst
                           (frame-stack-set-reg
                            frame-stack
                            dst
                            (vm-value-make-int (length (frame-return-values (car frame-stack)))))
                           '(:continue)))
          (scope-enter (frame-stack-push-scope frame-stack) '(:continue))
          (scope-exit (frame-stack-pop-scope frame-stack) '(:continue))
          (halt '(:done))
          (t (error 'vm-error
                    :message "unknown instruction" :instruction instruction)))
      (vm-divide-by-zero (e) `(:trap :divide-by-zero ,(vm-error-message e)))
      (vm-error (e) `(:trap :error ,(vm-error-message e)))
      )))

(defun run-program (&key program (frame-stack (list (make-frame))) trace report-state-callback)
  (let ((pc 0) (done nil))
    (loop while (and (not done) (< pc (length program))) do
      (let ((instruction (aref program pc)))
        (when trace
          (format t "~A: ~A~%" pc instruction))
        (if (eq (car instruction) 'report)
            (progn
              (when (and trace report-state-callback)
                (funcall report-state-callback frame-stack))
              (incf pc))
            (let ((control-directive (execute instruction frame-stack (1+ pc))))
              (case (car control-directive)
                (:continue (incf pc))
                (:jump (setf pc (cadr control-directive)))
                (:call
                 (setf pc (cadr control-directive))
                 (setf frame-stack (caddr control-directive)))
                (:return
                  (setf pc (cadr control-directive))
                  (setf frame-stack (caddr control-directive)))
                (:done (setf done t))
                (:trap
                 (format t "trap: ~A~%" (cdr control-directive))
                 (setf done t)))))))))

(defun fancy-run-program (&key program trace)
  (let ((frame-stack (list (make-frame))))
    (run-program
     :program program
     :frame-stack frame-stack
     :trace trace
     :report-state-callback (lambda (frame-stack)
                              (format t "~A" (frame-stack-str frame-stack))))))

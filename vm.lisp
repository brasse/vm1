(in-package :vm1)

(defstruct vm-state
  program
  pc
  frame-stack
  heap
  string-table)

(defun current-instruction ()
  (declare (special *instruction*))
  *instruction*)

(defun resolve-value (frame x string-table)
  (if (symbolp x)
      (frame-get-reg frame x)
      (vm-value-make-literal x :string-table string-table)))

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

(defun normal-call (frame-stack target args return-address)
  (if (integerp target)
      (values `(:call ,target) (frame-stack-push-frame frame-stack args return-address))
      (error 'vm-error :instruction (current-instruction)
                       :message (format nil "malformed target: ~A" target))))

(defun tail-call (frame-stack target args)
  (if (integerp target)
      (let ((top-frame (car frame-stack)))
        (setf (frame-args top-frame) args)
        (setf (frame-scopes top-frame) (list (make-hash-table)))
        `(:jump ,target))
      (error 'vm-error :instruction (current-instruction)
                       :message (format nil "malformed target: ~A" target))))

(defun ret (frame-stack return-address return-values)
  (let ((return-stack (frame-stack-pop-frame frame-stack)))
    (setf (frame-return-values (car return-stack)) return-values)
    (values `(:return ,return-address) return-stack)))

(defmacro args-1 (arg1 &body body)
  `(let ((,arg1 (cadr instruction)))
     ,@body))

(defmacro args-2 (arg1 arg2 &body body)
  `(let ((,arg1 (cadr instruction)) (,arg2 (caddr instruction)))
     ,@body))

(defmacro args-3 (arg1 arg2 arg3 &body body)
  `(let ((,arg1 (cadr instruction)) (,arg2 (caddr instruction)) (,arg3 (cadddr instruction)))
     ,@body))

(defmacro def-binop (fn)
  `(let ((dst (cadr instruction)) (a (caddr instruction)) (b (cadddr instruction)))
     (funcall set-reg dst (,fn (funcall resolve a) (funcall resolve b)))
     '(:continue)))

(defun execute (vm)
  (let* ((instruction (aref (vm-state-program vm) (vm-state-pc vm)))
         (return-address (1+ (vm-state-pc vm)))
         (frame-stack (vm-state-frame-stack vm))
         (string-table (vm-state-string-table vm))
         (head (car instruction))
         (resolve (lambda (x) (resolve-value (car frame-stack) x string-table)))
         (set-reg (lambda (dst value) (frame-set-reg (car frame-stack) dst value)))
         (*instruction* instruction))
    (declare (special *instruction*))
    (case head
      (const (args-2 dst literal
               (funcall set-reg
                        dst
                        (vm-value-make-literal literal :string-table string-table))
               '(:continue)))
      (mov (args-2 dst a
             (funcall set-reg dst (funcall resolve a))
             '(:continue)))

      (add (def-binop vm-value-add))
      (sub (def-binop vm-value-sub))
      (mul (def-binop vm-value-mul))
      (div (def-binop vm-value-div))
      (mod (def-binop vm-value-mod))
      (not (args-2 dst a
             (funcall set-reg
                      dst
                      (vm-value-not (funcall resolve a)))
             '(:continue)))

      (eq (def-binop vm-value-eq))
      (gt (def-binop vm-value-gt))
      (lt (def-binop vm-value-lt))

      (jmp (args-1 target (jmp target)))
      (jz (args-2 target condition (jz (funcall resolve condition) target)))
      (jnz (args-2 target condition (jnz (funcall resolve condition) target)))

      (call (destructuring-bind (target . args) (cdr instruction)
              (let ((resolved-values
                      (loop for arg in args collect (funcall resolve arg))))
                (multiple-value-bind (control-directive new-frame-stack)
                    (normal-call frame-stack target resolved-values return-address)
                  (setf (vm-state-frame-stack vm) new-frame-stack)
                  control-directive))))
      (tail-call (destructuring-bind (target . args) (cdr instruction)
                   (let ((resolved-values
                           (loop for arg in args collect (funcall resolve arg))))
                     (tail-call frame-stack target resolved-values))))
      (ret (let ((resolved-returns
                   (loop for ret in (cdr instruction) collect (funcall resolve ret))))
             (multiple-value-bind (control-directive new-frame-stack)
                 (ret frame-stack (frame-return-address (car frame-stack)) resolved-returns)
               (setf (vm-state-frame-stack vm) new-frame-stack)
               control-directive)))
      (get-arg (args-2 dst n
                 (funcall set-reg
                          dst
                          (frame-get-arg (car frame-stack) n))
                 '(:continue)))
      (get-ret (args-2 dst n
                 (funcall set-reg
                          dst
                          (nth n (frame-return-values (car frame-stack))))
                 '(:continue)))
      (get-ret-count (args-1 dst
                       (funcall set-reg
                                dst
                                (vm-value-make-int (length (frame-return-values (car frame-stack)))))
                       '(:continue)))
      (scope-enter (frame-push-scope (car frame-stack)) '(:continue))
      (scope-exit (frame-pop-scope (car frame-stack)) '(:continue))

      (print (args-1 a
               (format t "~A~%" (vm-value-str (funcall resolve a)))
               '(:continue)))

      (halt '(:done))
      (t (error 'vm-error
                :message "unknown instruction" :instruction instruction)))))

(defun run-program (vm &key trace report-state-callback)
  (let ((done nil))
    (loop while (and (not done) (< (vm-state-pc vm) (length (vm-state-program vm)))) do
      (let ((instruction (aref (vm-state-program vm) (vm-state-pc vm))))
        (when trace
          (format t "~A: ~A~%" (vm-state-pc vm) instruction))
        (if (eq (car instruction) 'report)
            (progn
              (when report-state-callback
                (funcall report-state-callback (vm-state-frame-stack vm)))
              (incf (vm-state-pc vm)))
            (let ((control-directive (execute vm)))
              (case (car control-directive)
                (:continue (incf (vm-state-pc vm)))
                ((:jump :call :return)
                 (setf (vm-state-pc vm) (cadr control-directive)))
                (:done (setf done t)))))))))

(defun fancy-run-program (&key program trace)
  (let ((vm (make-vm-state :program (assemble program)
                           :pc 0
                           :frame-stack (list (make-frame))
                           :string-table (make-hash-table))))
    (run-program
     vm
     :trace trace
     :report-state-callback (lambda (frame-stack)
                              (format t "~A" (frame-stack-str frame-stack))))))

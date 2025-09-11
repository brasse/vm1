(in-package :vm1)

(defstruct vm-state
  program
  pc
  frame-stack
  heap
  string-table
  struct-table)

(defun current-instruction ()
  (declare (special *instruction*))
  *instruction*)

(defun resolve-value (frame x string-table)
  (if (or (member x '(true false none))
          (integerp x)
          (stringp x))
      (vm-value-make-literal x :string-table string-table)
      (frame-get-reg frame x)))

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

(defun execute (vm)
  (let* ((instruction (aref (vm-state-program vm) (vm-state-pc vm)))
         (return-address (1+ (vm-state-pc vm)))
         (frame-stack (vm-state-frame-stack vm))
         (string-table (vm-state-string-table vm))
         (head (car instruction))
         (*instruction* instruction))
    (declare (special *instruction*))
    (flet ((args (n) (values-list (subseq instruction 1 (1+ n))))
           (resolve (x) (resolve-value (car frame-stack) x string-table))
           (set-reg (dst value) (frame-set-reg (car frame-stack) dst value)))
      (macrolet
          ((emit-binop (fn &key use-string-table)
             `(multiple-value-bind (dst a b) (args 3)
                (if ,use-string-table
                    (set-reg dst (,fn (resolve a)
                                      (resolve b)
                                      :string-table string-table))
                    (set-reg dst (,fn (resolve a) (resolve b))))
                '(:continue))))
        (case head
          (const (multiple-value-bind (dst literal) (args 2)
                   (set-reg
                    dst
                    (vm-value-make-literal literal :string-table string-table))
                   '(:continue)))
          (mov (multiple-value-bind (dst a) (args 2)
                 (set-reg dst (resolve a))
                 '(:continue)))

          (add (emit-binop vm-value-add))
          (sub (emit-binop vm-value-sub))
          (mul (emit-binop vm-value-mul))
          (div (emit-binop vm-value-div))
          (mod (emit-binop vm-value-mod))
          (not (multiple-value-bind (dst a) (args 2)
                 (set-reg
                  dst
                  (vm-value-not (resolve a)))
                 '(:continue)))

          (eq (emit-binop vm-value-eq))
          (gt (emit-binop vm-value-gt))
          (lt (emit-binop vm-value-lt))

          (jmp (multiple-value-bind (target) (args 1) (jmp target)))
          (jz (multiple-value-bind (target condition) (args 2)
                (jz (resolve condition) target)))
          (jnz (multiple-value-bind (target condition) (args 2)
                 (jnz (resolve condition) target)))

          (call (destructuring-bind (target . args) (cdr instruction)
                  (let ((resolved-values
                          (loop for arg in args collect (resolve arg))))
                    (multiple-value-bind (control-directive new-frame-stack)
                        (normal-call frame-stack target resolved-values return-address)
                      (setf (vm-state-frame-stack vm) new-frame-stack)
                      control-directive))))
          (tail-call (destructuring-bind (target . args) (cdr instruction)
                       (let ((resolved-values
                               (loop for arg in args collect (resolve arg))))
                         (tail-call frame-stack target resolved-values))))
          (ret (let ((resolved-returns
                       (loop for ret in (cdr instruction) collect (resolve ret))))
                 (multiple-value-bind (control-directive new-frame-stack)
                     (ret frame-stack (frame-return-address (car frame-stack)) resolved-returns)
                   (setf (vm-state-frame-stack vm) new-frame-stack)
                   control-directive)))
          (get-arg (multiple-value-bind (dst n) (args 2)
                     (set-reg
                      dst
                      (frame-get-arg (car frame-stack) n))
                     '(:continue)))
          (get-ret (multiple-value-bind (dst n) (args 2)
                     (set-reg
                      dst
                      (nth n (frame-return-values (car frame-stack))))
                     '(:continue)))
          (get-ret-count (multiple-value-bind (dst) (args 1)
                           (set-reg
                            dst
                            (vm-value-make-int (length (frame-return-values (car frame-stack)))))
                           '(:continue)))
          (scope-enter (frame-push-scope (car frame-stack)) '(:continue))
          (scope-exit (frame-pop-scope (car frame-stack)) '(:continue))

          (concat (emit-binop vm-value-concat :use-string-table t))
          (substr-start-end (multiple-value-bind
                                  (dst s start end) (args 4)
                              (set-reg
                               dst
                               (vm-value-substr
                                (resolve s)
                                (resolve start)
                                (resolve end)
                                :string-table string-table))
                              '(:continue)))
          (substr-start (multiple-value-bind (dst s start) (args 3)
                          (set-reg
                           dst
                           (vm-value-substr
                            (resolve s)
                            (resolve start)
                            +vm-value-none+
                            :string-table string-table))
                          '(:continue)))
          (strlen (multiple-value-bind (dst s) (args 2)
                    (set-reg
                     dst
                     (vm-value-strlen (resolve s)))
                    '(:continue)))
          (to-str (multiple-value-bind (dst x) (args 2)
                    (set-reg
                     dst
                     (vm-value-str (resolve x) :string-table string-table))
                    '(:continue)))
          (vec-make (multiple-value-bind (dst len) (args 2)
                      (set-reg
                       dst
                       (vm-value-make-array (list (resolve len))))
                      '(:continue)))
          (vec-len (multiple-value-bind (dst vec) (args 2)
                     (set-reg
                      dst
                      (vm-value-array-dim-size (resolve vec) (vm-value-make-int 0)))
                     '(:continue)))
          (vec-set (multiple-value-bind (vec idx val) (args 3)
                     (vm-value-array-set (resolve vec) (list (resolve idx)) (resolve val))
                     '(:continue)))
          (vec-get (multiple-value-bind (dst vec idx) (args 3)
                     (set-reg
                      dst
                      (vm-value-array-get (resolve vec) (list (resolve idx))))
                     '(:continue)))
          (map-make (multiple-value-bind (dst) (args 1)
                      (set-reg
                       dst
                       (vm-value-make-map))
                      '(:continue)))
          (map-set (multiple-value-bind (map key val) (args 3)
                     (vm-value-map-set (resolve map) (resolve key) (resolve val))
                     '(:continue)))
          (map-get (multiple-value-bind (dst map key) (args 3)
                     (set-reg
                      dst
                      (vm-value-map-get (resolve map) (resolve key)))
                     '(:continue)))
          (map-has (multiple-value-bind (dst map key) (args 3)
                     (set-reg
                      dst
                      (vm-value-map-has (resolve map) (resolve key)))
                     '(:continue)))

          (print (multiple-value-bind (a) (args 1)
                   (format t "~A~%" (%vm-value-to-string (resolve a) string-table))
                   '(:continue)))

          (halt '(:done))
          (t (error 'vm-error
                    :message "unknown instruction" :instruction instruction)))))))

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
                           :string-table (make-hash-table)
                           :struct-table (make-hash-table))))
    (run-program
     vm
     :trace trace
     :report-state-callback (lambda (frame-stack)
                              (format t "~A" (frame-stack-str frame-stack))))))

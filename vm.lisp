(in-package :vm1)

(defstruct vm-value
  type
  payload)

(defun make-vm-int (i)
  (make-vm-value :type :int :payload i))

(defun make-vm-string (s)
  (make-vm-value :type :string :payload s))

(defun make-vm-literal (x)
  (cond
    ((integerp x) (make-vm-int x))
    ((stringp x) (make-vm-string x))
    (t (error "unsupported literal: ~A" x))))

(defun print-reg-file (reg-file)
  (format t "~A"
          (loop for k being the hash-keys of reg-file using (hash-value v)
                collect (list k v))))

(defun current-reg-file ()
  (declare (special *reg-file*))
  *reg-file*)

(defun current-instruction ()
  (declare (special *instruction*))
  *instruction*)

(defun set-reg (reg val)
  (setf (gethash reg (current-reg-file)) val))

(defun get-reg (reg)
  (let ((val (gethash reg (current-reg-file))))
    (if val
        val
        (error 'vm-error :instruction (current-instruction)
                         :message (format nil "no such register: ~A" reg)))))

(defun get-value (x)
  (if (symbolp x)
      (get-reg x)
      (make-vm-literal x)))

(defmacro def-int-binary-op (name op)
  `(defun ,name (v1 v2)
     (let ((t1 (vm-value-type v1)) (t2 (vm-value-type v2))
           (v1 (vm-value-payload v1)) (v2 (vm-value-payload v2)))
       (unless (and (eq t1 :int) (eq t2 :int))
         (error 'vm-type-error :instruction (current-instruction) :expected :int  :actual (list t1 t2)))
       (make-vm-int (funcall ,op v1 v2)))))

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

(defmacro args-2 (arg1 arg2 &body body)
  `(let ((,arg1 (cadr instruction)) (,arg2 (caddr instruction)))
     ,@body))

(defmacro args-3 (arg1 arg2 arg3 &body body)
  `(let ((,arg1 (cadr instruction)) (,arg2 (caddr instruction)) (,arg3 (cadddr instruction)))
     ,@body))

(defun execute (instruction)
  (let ((head (car instruction)) (*instruction* instruction))
    (declare (special *instruction*))
    (case head
      (const (args-2 dst literal (set-reg dst (make-vm-literal literal))))
      (mov (args-2 dst a (set-reg dst (get-reg a))))
      (add (args-3 dst a b (set-reg dst (add (get-value a) (get-value b)))))
      (sub (args-3 dst a b (set-reg dst (sub (get-value a) (get-value b)))))
      (mul (args-3 dst a b (set-reg dst (mul (get-value a) (get-value b)))))
      ;; TODO: handle divide by zero
      (div (args-3 dst a b (set-reg dst (div (get-value a) (get-value b)))))
      (mod (args-3 dst a b (set-reg dst (vm1-mod (get-value a) (get-value b)))))
      (t (error 'vm-error :message "unknown instruction" :instruction instruction))
      )))

(defun run-program (&key (program '()) (reg-file (make-hash-table)))
  (let ((*reg-file* reg-file))
    (declare (special *reg-file*))
    (loop for instruction in program do
      (execute instruction))))

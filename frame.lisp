(in-package :vm1)

(defstruct (frame (:constructor %make-frame))
  args
  return-address
  return-values
  scopes)

(defun make-frame (&key (args nil) (return-address nil) (scopes nil))
  (%make-frame :args args
               :return-address return-address
               :return-values nil
               :scopes scopes))

(defun frame-push-scope (frame)
  (push (make-hash-table) (frame-scopes frame)))

(defun frame-pop-scope (frame)
  (pop (frame-scopes frame)))

(defun frame-set-reg (frame reg value)
  (let ((scopes (frame-scopes frame)))
    (if scopes
        (setf (gethash reg (car scopes)) value)
        (error 'vm-error :instruction (current-instruction)
                         :message "no scope in frame"))))

(defun frame-get-reg (frame reg)
  (loop for scope in (frame-scopes frame) do
    (let ((val (gethash reg scope)))
      (when val (return-from frame-get-reg val))))
  (error 'vm-error :instruction (current-instruction)
                   :message (format nil "no such register ~A" reg)))

(defun frame-get-arg (frame n)
  (nth n (frame-args frame)))

(defun %scope-str (scope)
  (format nil "[~{~A~^ ~}]"
          (loop for k being the hash-keys of scope using (hash-value v)
                collect (format nil "~A:~A" k (vm-value-str v)))))

(defun frame-str (frame)
  (format nil "~{~A~%~}args: ~A~%return-address: ~A~%return-values: ~A~%"
          (loop
            for i from 0
            for scope in (reverse (frame-scopes frame))
            collect (format nil "~A ~A" i (%scope-str scope)))
          (format nil "[~{~A~^ ~}]"
                  (loop
                    for i from 0
                    for arg in (frame-args frame)
                    collect (format nil "~A:~A" i (vm-value-str arg))))
          (frame-return-address frame)
          (format nil "[~{~A~^ ~}]"
                  (loop
                    for i from 0
                    for arg in (frame-return-values frame)
                    collect (format nil "~A:~A" i (vm-value-str arg))))))


(defun frame-stack-push-frame (frame-stack args return-address)
  (cons (make-frame :args args :return-address return-address) frame-stack))

(defun frame-stack-pop-frame (frame-stack)
  (cdr frame-stack))

(defun frame-stack-push-scope (frame-stack)
  (frame-push-scope (car frame-stack)))

(defun frame-stack-pop-scope (frame-stack)
  (frame-pop-scope (car frame-stack)))

(defun frame-stack-set-reg (frame-stack reg value)
  (frame-set-reg (car frame-stack) reg value))

(defun frame-stack-get-reg (frame-stack reg)
  (frame-get-reg (car frame-stack) reg))

(defun frame-stack-get-arg (frame-stack n)
  (frame-get-arg (car frame-stack) n))

(defun frame-stack-get-return-address (frame-stack)
  (frame-return-address (car frame-stack)))

(defun frame-stack-set-return-values (frame-stack return-values)
  (setf (frame-return-values (car frame-stack)) return-values))

(defun frame-stack-get-return-value (frame-stack n)
  (nth n (frame-return-values (car frame-stack))))

(defun frame-stack-str (frame-stack)
  (format nil "~{~A~%~}"
          (loop
            for i from 0
            for frame in (reverse frame-stack)
            collect (format nil "- frame ~A -~%~A" i (frame-str frame)))))

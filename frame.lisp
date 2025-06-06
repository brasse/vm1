(in-package :vm1)

(defstruct (frame (:constructor %make-frame))
  args
  return-address
  return-values
  scopes)

(defun make-frame (&key args return-address)
  (%make-frame :args args
               :return-address return-address
               :return-values nil
               :scopes (list (make-hash-table))))

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
                collect (format nil "~A:~A" k (%vm-value-str v)))))

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
                    collect (format nil "~A:~A" i (%vm-value-str arg))))
          (frame-return-address frame)
          (format nil "[~{~A~^ ~}]"
                  (loop
                    for i from 0
                    for arg in (frame-return-values frame)
                    collect (format nil "~A:~A" i (%vm-value-str arg))))))


(defun frame-stack-push-frame (frame-stack args return-address)
  (cons
   (make-frame :args args :return-address return-address)
   frame-stack))

(defun frame-stack-pop-frame (frame-stack)
  (cdr frame-stack))

(defun frame-stack-str (frame-stack)
  (format nil "~{~A~%~}"
          (loop
            for i from 0
            for frame in (reverse frame-stack)
            collect (format nil "- frame ~A -~%~A" i (frame-str frame)))))

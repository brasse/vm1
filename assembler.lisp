(in-package :vm1)

(defun assemble (program)
  (let ((the-labels (make-hash-table)))
    ;; resolve label addresses
    (loop with addr = 0
          for instruction across program
          do (case (car instruction)
               (label (setf (gethash (cadr instruction) the-labels) addr))
               (t (incf addr))))
    ;; resolve jump labels
    (coerce
     (loop for instruction across program
           for x = (case (car instruction)
                     (label nil)
                     ((jmp jz jnz call)
                      (let ((head (car instruction))
                            (tail (cddr instruction))
                            (target (gethash (cadr instruction) the-labels)))
                        (unless target
                          (error 'asm-error :instruction instruction
                                            :message "undefined label"))
                        (append (list head target) tail)))
                     (t instruction))
           when x collect x)
     'vector)))

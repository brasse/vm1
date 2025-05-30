(in-package :vm1)

(define-condition vm-internal-error (error)
  ((message :initarg :message :reader vm-internal-error-message))
  (:report
   (lambda (c s)
     (format s "internal vm error: ~A" (vm-internal-error-message c)))))

(define-condition vm-error (error)
  ((instruction :initarg :instruction :reader vm-error-instruction)
   (message :initarg :message :initform nil :reader vm-error-message))
  (:report
   (lambda (c s)
     (format s "VM error in ~S: ~A"
             (vm-error-instruction c)
             (vm-error-message c)))))

(define-condition vm-divide-by-zero (vm-error) ()
  (:report (lambda (c s)
             (format s "divide by zero in ~S" (vm-error-instruction c)))))

(define-condition vm-type-error (vm-error)
  ((expected :initarg :expected :reader vm-error-expected)
   (actual :initarg :actual :reader vm-error-actual))
  (:report
   (lambda (c s)
     (format s "type error in ~S: expected ~A, got ~A"
             (vm-error-instruction c)
             (vm-error-expected c)
             (vm-error-actual c)))))

(define-condition asm-error (error)
  ((instruction :initarg :instruction :reader asm-error-instruction)
   (message :initarg :message :initform nil :reader asm-error-message))
  (:report
   (lambda (c s)
     (format s "ASM error in ~S: ~A"
             (asm-error-instruction c)
             (asm-error-message c)))))

(defmacro vm-assert (condition &optional (message "no message"))
  `(unless ,condition
     (error 'vm-internal-error :message
            (format nil "~A (failed condition: ~S)" ,message ',condition))))

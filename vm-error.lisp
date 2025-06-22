(in-package :vm1)

(define-condition vm-internal-error (error)
  ((message :initarg :message :reader vm-internal-error-message))
  (:report
   (lambda (c s)
     (format s "~A" (vm-internal-error-message c)))))

(define-condition vm-error (error)
  ((instruction :initarg :instruction :reader vm-error-instruction)
   (message :initarg :message :initform nil :reader vm-error-message))
  (:report
   (lambda (c s)
     (format s "~A: ~S"
             (vm-error-message c)
             (vm-error-instruction c)))))

(define-condition vm-index-out-of-bounds (vm-error) ())

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

(define-condition compiler-error (error)
  ((message :initarg :message :reader compiler-error-message)
   (node :initarg :node :reader compiler-error-node))
  (:report
   (lambda (c s)
     (format s "~A~%node: ~A"
             (compiler-error-message c)
             (compiler-error-node c)))))

(define-condition compiler-type-error (compiler-error)
  ((expected :initarg :expected :reader compiler-type-error-expected)
   (actual :initarg :actual :reader compiler-type-error-actual))
  (:report
   (lambda (c s)
     (format s "~A~%node: ~A~%expected: ~A~%actual: ~A"
             (compiler-error-message c)
             (compiler-error-node c)
             (compiler-type-error-expected c)
             (compiler-type-error-actual c)))))

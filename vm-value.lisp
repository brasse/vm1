(in-package :vm1)

(defstruct vm-value
  type
  payload)

(defconstant +vm-value-false+
  (make-vm-value :type :bool :payload nil))

(defconstant +vm-value-true+
  (make-vm-value :type :bool :payload t))

(defun vm-value-make-int (i)
  (make-vm-value :type :int :payload i))

(defun vm-value-make-string (s)
  (make-vm-value :type :string :payload s))

(defun vm-value-make-literal (x)
  (cond
    ((integerp x) (vm-value-make-int x))
    ((stringp x) (vm-value-make-string x))
    ((eq x 'true) +vm-value-true+)
    ((eq x 'false) +vm-value-false+)
    (t (error "unsupported literal: ~A" x))))

(defun vm-value-falsep (x)
  (let ((payload (vm-value-payload x)))
    (case (vm-value-type x)
      (:int (= payload 0))
      (:string (string= payload ""))
      (:bool (not payload)))))

(defun vm-value-str (x)
  (let ((type (vm-value-type x))
        (payload (vm-value-payload x)))
    (format nil "~A:~A"
            (case type
              (:int "i")
              (:string "s")
              (:bool "b"))
            (case type
              ((:int :string) payload)
              (:bool (if payload "true" "false"))))))

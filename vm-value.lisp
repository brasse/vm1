(in-package :vm1)

(defstruct vm-value
  type
  payload)

(defun vm-value-make-int (i)
  (make-vm-value :type :int :payload i))

(defun vm-value-make-string (s)
  (make-vm-value :type :string :payload s))

(defun vm-value-make-literal (x)
  (cond
    ((integerp x) (vm-value-make-int x))
    ((stringp x) (vm-value-make-string x))
    (t (error "unsupported literal: ~A" x))))

(defun vm-value-falsep (x)
  (let ((payload (vm-value-payload x)))
    (case (vm-value-type x)
      (:int (= payload 0))
      (:string (string= payload "")))))

(defun vm-value-str (x)
  (format nil "~A:~A"
          (case (vm-value-type x)
            (:int "i")
            (:string "s"))
          (vm-value-payload x)))

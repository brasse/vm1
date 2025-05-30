(in-package :vm1)

(defstruct vm-value
  type
  payload)

(defparameter +vm-value-false+
  (make-vm-value :type :bool :payload nil))

(defparameter +vm-value-true+
  (make-vm-value :type :bool :payload t))

(defun vm-value-make-int (i)
  (make-vm-value :type :int :payload i))

(defun %vm-value-intern-string (s string-table)
  (or (gethash s string-table)
      (setf (gethash s string-table) s)))

(defun vm-value-make-string (s string-table)
  (make-vm-value :type :string :payload (%vm-value-intern-string s string-table)))

(defun vm-value-make-literal (x &key string-table)
  (cond
    ((integerp x) (vm-value-make-int x))
    ((stringp x) (progn
                   (vm-assert string-table
                              "string-table must be provided for string literals")
                   (vm-value-make-string x string-table)))
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

(defun vm-value-not (a)
  (if (vm-value-falsep a) +vm-value-true+ +vm-value-false+))

(defmacro def-int-binary-op (name op)
  `(defun ,name (v1 v2)
     (let ((t1 (vm-value-type v1)) (t2 (vm-value-type v2))
           (v1 (vm-value-payload v1)) (v2 (vm-value-payload v2)))
       (unless (and (eq t1 :int) (eq t2 :int))
         (error 'vm-type-error :instruction (current-instruction)
                               :expected :int  :actual (list t1 t2)))
       (vm-value-make-int (funcall ,op v1 v2)))))

(def-int-binary-op vm-value-add (lambda (a b) (+ a b)))
(def-int-binary-op vm-value-sub (lambda (a b) (- a b)))
(def-int-binary-op vm-value-mul (lambda (a b) (* a b)))
(def-int-binary-op vm-value-div
    (lambda (a b) (if (zerop b)
                      (error 'vm-divide-by-zero :instruction (current-instruction))
                      (truncate a b))))
(def-int-binary-op vm-value-mod
    (lambda (a b) (if (zerop b)
                      (error 'vm-divide-by-zero :instruction (current-instruction))
                      (nth-value 1 (truncate a b)))))

(defmacro with-same-type+payload (v1 v2 &body body)
  `(let ((t1 (vm-value-type ,v1))
         (t2 (vm-value-type ,v2)))
     (unless (eq t1 t2)
       (error 'vm-type-error
              :instruction (current-instruction)
              :expected :same-type
              :actual (list t1 t2)))
     (let ((p1 (vm-value-payload ,v1))
           (p2 (vm-value-payload ,v2)))
       ,@body)))

(Defun vm-value-eq (a b)
  (with-same-type+payload a b
    (if (case t1
          (:int (= p1 p2))
          ;; this works because all strings are interned
          ;; and true and false are constants
          ((:string :bool) (eq p1 p2)))
        +vm-value-true+
        +vm-value-false+)

    (if (equal p1 p2) +vm-value-true+ +vm-value-false+)))

(defun vm-value-gt (a b)
  (with-same-type+payload a b
    (if (case t1
          (:int (> p1 p2))
          (:string (string> p1 p2))
          (:bool (and (eq p1 t) (eq p2 nil))))
        +vm-value-true+
        +vm-value-false+)))

(defun vm-value-lt (a b)
  (with-same-type+payload a b
    (if (case t1
          (:int (< p1 p2))
          (:string (string< p1 p2))
          (:bool (and (eq p1 nil) (eq p2 t))))
        +vm-value-true+
        +vm-value-false+)))

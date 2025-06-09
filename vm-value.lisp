(in-package :vm1)

(defstruct vm-value
  type
  payload)

(defparameter +vm-value-false+
  (make-vm-value :type :bool :payload nil))

(defparameter +vm-value-true+
  (make-vm-value :type :bool :payload t))

(defparameter +vm-value-none+
  (make-vm-value :type :none :payload nil))

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
    ((eq x 'none) +vm-value-none+)
    (t (error "unsupported literal: ~A" x))))

(defun vm-value-falsep (x)
  (let ((payload (vm-value-payload x)))
    (case (vm-value-type x)
      (:int (= payload 0))
      (:string (string= payload ""))
      (:bool (not payload))
      (:none t))))

(defun vm-value-str (x)
  (let ((type (vm-value-type x))
        (payload (vm-value-payload x)))
    (format nil "~A"
            (case type
              ((:int :string) payload)
              (:bool (if payload "true" "false"))
              (:none "none")))))

(defun %vm-value-str (x)
  (let ((type (vm-value-type x))
        (payload (vm-value-payload x)))
    (format nil "~A:~A"
            (case type
              (:int "i")
              (:string "s")
              (:bool "b")
              (:none "n"))
            (case type
              ((:int :string) payload)
              (:bool (if payload "true" "false"))
              (:none "none")))))

(defun vm-value-not (a)
  (if (vm-value-falsep a) +vm-value-true+ +vm-value-false+))

(defmacro def-vm-value-op (name expected-types result-type fn)
  ;; name of function
  ;; list of expected-types for arguments of function
  ;; the result-type of the function
  ;; fn is the function taking same number of arguments as types in expected-types
  ;; fn will grow a key argument, string-table, if the result type is :string
  (let* ((arg-names (loop for i from 1 to (length expected-types)
                          collect (make-symbol (format nil "a~A" i))))
         (full-args (if (eq :string result-type)
                        (append arg-names '(&key string-table))
                        arg-names)))
    `(defun ,name ,full-args
       (let ((actual-types (mapcar #'vm-value-type (list ,@arg-names))))
         (unless (equal actual-types  ',expected-types)
           (error 'vm-type-error :instruction (current-instruction)
                                 :expected ',expected-types  :actual actual-types))
         (let ((result (apply ,fn (mapcar #'vm-value-payload (list ,@arg-names)))))
           (if (eq :string ,result-type)
               (if string-table
                   (vm-value-make-string result string-table)
                   (error 'vm-internal-error :message (format nil "~A requires a string table" ',name)))
               (make-vm-value :type ,result-type
                              :payload result)))))))

(def-vm-value-op vm-value-add (:int :int) :int #'+)
(def-vm-value-op vm-value-sub (:int :int) :int #'-)
(def-vm-value-op vm-value-mul (:int :int) :int #'*)
(def-vm-value-op vm-value-div (:int :int) :int
  (lambda (a b) (if (zerop b)
                    (error 'vm-divide-by-zero :instruction (current-instruction))
                    (truncate a b))))
(def-vm-value-op vm-value-mod (:int :int) :int
  (lambda (a b) (if (zerop b)
                    (error 'vm-divide-by-zero :instruction (current-instruction))
                    (nth-value 1 (truncate a b)))))


(def-vm-value-op vm-value-concat (:string :string) :string
  (lambda (a b) (concatenate 'string a b)))

(def-vm-value-op vm-value-strlen (:string) :int #'length)

(defun vm-value-substr (s start end string-table)
  (let ((s-type (vm-value-type s))
        (start-type (vm-value-type start))
        (end-type (vm-value-type end))
        (str (vm-value-payload s))
        (start (vm-value-payload start))
        (end (vm-value-payload end))) ;; if :none this payload will be nil
    (unless (and (eq s-type :string)
                 (eq start-type :int)
                 (or (eq end-type :none) (eq end-type :int)))
      (error 'vm-type-error :instruction (current-instruction)
                            :expected (list :string :int :int)
                            :actual (list s-type start-type end-type)))
    (vm-value-make-string (subseq str start end) string-table)))

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
          ((:string :bool) (eq p1 p2))
          (:none t))
        +vm-value-true+
        +vm-value-false+)))

(defun vm-value-gt (a b)
  (with-same-type+payload a b
    (if (case t1
          (:int (> p1 p2))
          (:string (string> p1 p2))
          (:bool (and (eq p1 t) (eq p2 nil)))
          (:none nil))
        +vm-value-true+
        +vm-value-false+)))

(defun vm-value-lt (a b)
  (with-same-type+payload a b
    (if (case t1
          (:int (< p1 p2))
          (:string (string< p1 p2))
          (:bool (and (eq p1 nil) (eq p2 t)))
          (:none nil))
        +vm-value-true+
        +vm-value-false+)))

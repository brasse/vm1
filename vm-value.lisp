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

(defun vm-value-make-array (dimensions)
  (make-vm-value :type :array
                 :payload (make-array dimensions :initial-element +vm-value-none+)))

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

(defun vm-value-substr (s start end &key string-table)
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

(defun %validate-pos (arr pos)
  (when (not (= (length pos) (array-rank arr)))
    (error 'vm-index-out-of-bounds
           :instruction (current-instruction)
           :message (format nil
                            "wrong number of subscripts, ~A, for array of rank ~A."
                            (length pos) (array-rank arr))))
  (loop for d from 0
        for i in pos do
          (if (or (< i 0) (>= i (array-dimension arr d)))
              (error 'vm-index-out-of-bounds
                     :instruction (current-instruction)
                     :message (format nil
                                      "invalid index ~A for axis ~A of array, should be 0 >= i < ~A."
                                      i d (array-dimension arr d))))))

(defun vm-value-array-set (arr pos x)
  ;; check array type
  (let ((type (vm-value-type arr)))
    (unless (eq :array type)
      (error 'vm-type-error :instruction (current-instruction)
                            :expected :array :actual type)))
  (unless (vm-value-p x)
    (error 'vm-internal-error :message "can only insert vm-value into array"))

  (let* ((actual-array (vm-value-payload arr))
         (rank (array-rank actual-array))
         (pos-rank (length pos))
         (pos-types (mapcar #'vm-value-type pos)))
    ;; check pos has same rank as array
    (unless  (= pos-rank rank)
      (error 'vm-index-out-of-bounds
             :instruction (current-instruction)
             :message (format nil
                              "wrong number of subscripts, ~A, for array of rank ~A."
                              pos-rank rank)))
    ;; check pos is all :int
    (unless (every (lambda (type) (eq :int type)) pos-types)
      (error 'vm-type-error :instruction (current-instruction)
                            :expected (make-list rank :initial-element :int)
                            :actual pos-types))
    ;; check subcomponents of pos
    (loop for d from 0
          for i in (mapcar #'vm-value-payload pos) do
            (let ((dim (array-dimension actual-array d)))
              (if (or (< i 0) (>= i dim))
                  (error 'vm-index-out-of-bounds
                         :instruction (current-instruction)
                         :message (format nil
                                          "invalid index ~A for axis ~A of array, should be 0 >= i < ~A."
                                          i d dim)))))
    (setf (apply #'aref (cons actual-array (mapcar #'vm-value-payload pos))) x)))

(defun vm-value-array-get (arr pos)
  (let ((type (vm-value-type arr)))
    (unless (eq :array type)
      (error 'vm-type-error :instruction (current-instruction)
                            :expected :array :actual type)))
  (let ((actual-array (vm-value-payload arr)))
    (%validate-pos actual-array pos)
    (apply #'aref (cons actual-array pos))))

(def-vm-value-op vm-value-array-rank (:array) :int #'array-rank)

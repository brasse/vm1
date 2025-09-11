(in-package :vm1)

(defstruct vm-value
  type ;; :int, :string, :bool, :none, :array, :struct, :map, :symbol
  payload

  ;; Each struct gets its own unique ID. The VM will make no
  ;; difference between arrays and structs. Structs are implemented
  ;; using arrays. The VM will inspect the id when getting and setting
  ;; fields and verifying that the fields exist using struct definitions
  ;; inside a struct table.
  id)

(declaim (inline %assert-type))
(defun %assert-type (value expected)
  (unless
      (if (consp expected)
          (member (vm-value-type value) expected)
          (eq (vm-value-type value) expected))
    (error 'vm-type-error
           :instruction (current-instruction)
           :expected    expected
           :actual      (vm-value-type value))))

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

(defun vm-value-make-symbol (symbol)
  (make-vm-value :type :symbol :payload symbol))

(defun vm-value-make-array (dimensions)
  (let ((rank (length dimensions))
        (dimension-types (mapcar #'vm-value-type dimensions))
        (dimension-values (mapcar #'vm-value-payload dimensions)))
    (unless (every (lambda (type) (eq :int type)) dimension-types)
      (error 'vm-type-error :instruction (current-instruction)
                            :expected (make-list rank :initial-element :int)
                            :actual dimension-types))
    (make-vm-value :type :array
                   :payload (make-array dimension-values :initial-element +vm-value-none+))))

(defun vm-value-make-map ()
  (make-vm-value :type :map
                 :payload (make-hash-table :test #'equal)))

(defun vm-value-make-literal (x &key string-table)
  (cond
    ((integerp x) (vm-value-make-int x))
    ((stringp x) (progn
                   (vm-assert string-table
                       "string-table must be provided for string literals")
                   (vm-value-make-string x string-table)))
    ((keywordp x) (vm-value-make-symbol x))
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
      (:array (zerop (array-total-size payload)))
      (:struct nil)
      (:map (zerop (hash-table-count payload)))
      (:symbol nil)
      (:none t))))

(defun %vm-value-to-string (x &optional string-table)
  (let ((type (vm-value-type x))
        (payload (vm-value-payload x)))
    (format nil "~A"
            (case type
              ((:int :string) payload)
              (:bool (if payload "true" "false"))
              (:symbol (symbol-name payload))
              (:array (if (= 1 (array-rank payload))
                          (format nil "[~{~A~^, ~}]"
                                  (map 'list (lambda (x) (%vm-value-to-string x string-table))
                                       payload))
                          "[ sorry, print not implemented for n-dimensional arrays ]"))
              (:struct
               (format nil "#<struct ~A {~{~A~^, ~}}>"
                       (vm-value-id x)
                       (map 'list (lambda (x) (%vm-value-to-string x string-table)) payload)))
              (:map
               (format nil "{~{~A: ~A~^, ~}}"
                       (loop for k being the hash-keys of payload using (hash-value v)
                             ;; reconstruct the vm-value from the key
                             ;; it might be a string so that's why we need the string-table
                             collect (%vm-value-to-string
                                      (%key->vm-value k string-table)
                                      string-table)
                             collect (%vm-value-to-string v))))
              (:none "none")))))

(defun %vm-value-str (x)
  (let ((type (vm-value-type x))
        (payload (vm-value-payload x)))
    (format nil "~A:~A"
            (case type
              (:int "i")
              (:string "s")
              (:bool "b")
              (:symbol "sym")
              (:none "n"))
            (case type
              ((:int :string) payload)
              (:bool (if payload "true" "false"))
              (:symbol (symbol-name payload))
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

(defun vm-value-str (x &key string-table)
  (vm-assert string-table "string-table must be provided for vm-value-str")
  (vm-value-make-string (%vm-value-to-string x string-table) string-table))

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

(defun vm-value-eq (a b)
  (with-same-type+payload a b
    (if (case t1
          (:int (= p1 p2))
          ;; this works because all strings are interned
          ;; and true and false are constants
          ((:string :bool :symbol) (eq p1 p2))
          (:none t))
        +vm-value-true+
        +vm-value-false+)))

(defun vm-value-gt (a b)
  (with-same-type+payload a b
    (if (case t1
          (:int (> p1 p2))
          (:string (string> p1 p2))
          (:symbol (string> (symbol-name p1) (symbol-name p2)))
          (:bool (and (eq p1 t) (eq p2 nil)))
          (:none nil))
        +vm-value-true+
        +vm-value-false+)))

(defun vm-value-lt (a b)
  (with-same-type+payload a b
    (if (case t1
          (:int (< p1 p2))
          (:string (string< p1 p2))
          (:symbol (string< (symbol-name p1) (symbol-name p2)))
          (:bool (and (eq p1 nil) (eq p2 t)))
          (:none nil))
        +vm-value-true+
        +vm-value-false+)))

(defun %decode-indices (arr indices)
  (%assert-type arr '(:array :struct))

  (let* ((actual-array (vm-value-payload arr))
         (rank (array-rank actual-array))
         (indices-rank (length indices))
         (indices-types (mapcar #'vm-value-type indices)))
    ;; check indices has same rank as array
    (unless  (= indices-rank rank)
      (error 'vm-index-out-of-bounds
             :instruction (current-instruction)
             :message (format nil
                              "wrong number of subscripts, ~A, for array of rank ~A."
                              indices-rank rank)))
    ;; check indices are all :int
    (unless (every (lambda (type) (eq :int type)) indices-types)
      (error 'vm-type-error :instruction (current-instruction)
                            :expected (make-list rank :initial-element :int)
                            :actual indices-types))
    ;; check subcomponents of indices
    (loop for d from 0
          for i in (mapcar #'vm-value-payload indices) do
            (let ((dim (array-dimension actual-array d)))
              (if (or (< i 0) (>= i dim))
                  (error 'vm-index-out-of-bounds
                         :instruction (current-instruction)
                         :message (format nil
                                          "invalid index ~A for axis ~A of array, should be 0 >= i < ~A."
                                          i d dim)))))
    (values actual-array  (mapcar #'vm-value-payload indices))))

(defun vm-value-array-set (arr indices x)
  (unless (vm-value-p x)
    (error 'vm-internal-error :message "can only insert vm-value into array"))
  (multiple-value-bind (actual-array actual-indices) (%decode-indices arr indices)
    (setf (apply #'aref (cons actual-array actual-indices)) x)))

(defun vm-value-array-get (arr indices)
  (multiple-value-bind (actual-array actual-indices) (%decode-indices arr indices)
    (apply #'aref (cons actual-array actual-indices))))

(def-vm-value-op vm-value-array-rank (:array) :int #'array-rank)

(defun vm-value-array-dim-size (arr axis)
  (let ((arr-type (vm-value-type arr))
        (arr-value (vm-value-payload arr))
        (axis-type (vm-value-type axis))
        (axis-value (vm-value-payload axis)))
    (unless (equal (list arr-type axis-type) '(:array :int))
      (error 'vm-type-error :instruction (current-instruction)
                            :expected '(:array :int) :actual (list arr-type axis-type)))
    (let ((arr-rank (array-rank arr-value)))
      (unless (and (>= axis-value 0) (< axis-value arr-rank))
        (error 'vm-index-out-of-bounds
               :instruction (current-instruction)
               :message (format nil
                                "invalid axis index ~A for array with rank ~A"
                                axis-value arr-rank)))
      (vm-value-make-int (array-dimension arr-value axis-value)))))

(defun %vm-value->key (key)
  (unless (vm-value-p key)
    (error 'vm-internal-error :message "can only use vm-value as key into map"))
  (let ((type (vm-value-type key)) (payload (vm-value-payload key)))
    (if (member type '(:int :string :bool :none :symbol))
        (list type payload)
        (error 'vm-internal-error
               :message "only scalar vm-values can be used as key into map"))))

(defun %key->vm-value (key string-table)
  (destructuring-bind (type payload) key
    (ecase type
      (:int    (vm-value-make-int payload))
      (:string (vm-value-make-string payload string-table))
      (:bool   (if payload +vm-value-true+ +vm-value-false+))
      (:symbol (vm-value-make-symbol payload))
      (:none   +vm-value-none+))))

(defun vm-value-map-has (map key)
  (%assert-type map :map)
  (if (nth-value 1 (gethash (%vm-value->key key) (vm-value-payload map)))
      +vm-value-true+
      +vm-value-false+))

(defun vm-value-map-set (map key value)
  (%assert-type map :map)
  (unless (vm-value-p value)
    (error 'vm-internal-error :message "can only use vm-value as value into map"))
  (setf (gethash (%vm-value->key key) (vm-value-payload map)) value))

(defun vm-value-map-get (map key)
  (%assert-type map :map)
  (multiple-value-bind (value present-p)
      (gethash (%vm-value->key key) (vm-value-payload map))
    (if present-p
        value
        +vm-value-none+)))

(defun vm-value-define-struct (struct-table struct-id field-ids)
  (if (nth-value 1 (gethash struct-id struct-table))
      (error 'vm-internal-error
             :message (format nil "struct with id ~A already defined" struct-id))
      (setf (gethash struct-id struct-table)
            (loop for field-id in field-ids
                  for i from 0
                  with field-map = (make-hash-table)
                  do (setf (gethash field-id field-map) i)
                  finally (return field-map)))))

(defun vm-value-make-struct (struct-table struct-id)
  (multiple-value-bind (field-ids present-p) (gethash struct-id struct-table)
    (unless present-p
      (error 'vm-internal-error
             :message (format nil "no struct definition with id ~A" struct-id)))
    (make-vm-value :type :struct
                   :id struct-id
                   :payload (make-array
                             (hash-table-count field-ids)
                             :initial-element +vm-value-none+))))

(defun vm-value-struct-get (struct-table struct field-id)
  (%assert-type struct :struct)
  (let ((struct-id (vm-value-id struct)))
    (multiple-value-bind (field-ids present-p) (gethash struct-id struct-table)
      (unless present-p
        (error 'vm-internal-error
               :message (format nil "no struct definition with id ~A" struct-id)))
      (multiple-value-bind (field-idx present-p) (gethash field-id field-ids)
        (unless present-p
          (error 'vm-internal-error
                 :message (format nil "no field ~A in struct ~A" field-id struct-id)))
        (vm-value-array-get struct (list (vm-value-make-int field-idx)))))))

(defun vm-value-struct-set (struct-table struct field-id value)
  (%assert-type struct :struct)
  (unless (vm-value-p value)
    (error 'vm-internal-error :message "can only use vm-value as value into struct"))
  (let ((struct-id (vm-value-id struct)))
    (multiple-value-bind (field-ids present-p) (gethash struct-id struct-table)
      (unless present-p
        (error 'vm-internal-error
               :message (format nil "no struct definition with id ~A" struct-id)))
      (multiple-value-bind (field-idx present-p) (gethash field-id field-ids)
        (unless present-p
          (error 'vm-internal-error
                 :message (format nil "no field ~A in struct ~A" field-id struct-id)))
        (vm-value-array-set struct (list (vm-value-make-int field-idx)) value)))))

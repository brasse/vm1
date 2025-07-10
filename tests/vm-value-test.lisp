(in-package :vm1)

(def-suite vm-value-test)

(in-suite vm-value-test)

(defparameter *instruction* nil)

(test make-int-literal
  (let ((a (vm-value-make-literal 123)))
    (is (eq :int (vm-value-type a)))
    (is (= 123 (vm-value-payload a)))))

(test make-bool-literal
  (let ((a (vm-value-make-literal 'false)))
    (is (eq :bool (vm-value-type a)))
    (is (eq nil (vm-value-payload a))))

  (let ((a (vm-value-make-literal 'true)))
    (is (eq :bool (vm-value-type a)))
    (is (eq t (vm-value-payload a)))))

(test make-string-literal
  (let ((a (vm-value-make-literal "foo" :string-table (make-hash-table))))
    (is (eq :string (vm-value-type a)))
    (is (string= "foo" (vm-value-payload a)))))

(test make-string-literal-fails-without-string-table
  (signals vm-internal-error (vm-value-make-literal "foo")))

(test string-are-interned
  (let* ((string-table (make-hash-table))
         (a (vm-value-make-literal "foo" :string-table string-table))
         (b (vm-value-make-literal "foo" :string-table string-table)))
    (is (eq (vm-value-payload a) (vm-value-payload b)))))

(test make-none-literal
  (let ((a (vm-value-make-literal 'none)))
    (is (eq :none (vm-value-type a)))
    (is (null (vm-value-payload a)))))

(test not-does-its-thing
  (let ((a (vm-value-not (vm-value-make-literal 0))))
    (is (eq +vm-value-true+ a)))
  (let ((a (vm-value-not (vm-value-make-literal "" :string-table (make-hash-table)))))
    (is (eq +vm-value-true+ a)))
  (let ((a (vm-value-not +vm-value-false+)))
    (is (eq +vm-value-true+ a)))
  (let ((a (vm-value-not +vm-value-none+)))
    (is (eq +vm-value-true+ a)))

  (let ((a (vm-value-not (vm-value-make-literal 42))))
    (is (eq +vm-value-false+ a)))
  (let ((a (vm-value-not (vm-value-make-literal "x" :string-table (make-hash-table)))))
    (is (eq +vm-value-false+ a)))
  (let ((a (vm-value-not +vm-value-true+)))
    (is (eq +vm-value-false+ a))))

(test add-ints
  (let ((a (vm-value-add (vm-value-make-literal 100) (vm-value-make-literal 23))))
    (is (eq :int (vm-value-type a)))
    (is (= 123 (vm-value-payload a)))))

(test add-ints-only-for-ints
  (signals vm-type-error
    (vm-value-add
     (vm-value-make-literal 100)
     (vm-value-make-literal 'false)))
  (signals vm-type-error
    (vm-value-add
     (vm-value-make-literal 100)
     (vm-value-make-literal "foo" :string-table (make-hash-table)))))

(test sub-ints
  (let ((a (vm-value-sub (vm-value-make-literal 100) (vm-value-make-literal 23))))
    (is (eq :int (vm-value-type a)))
    (is (= 77 (vm-value-payload a)))))

(test mul-ints
  (let ((a (vm-value-mul (vm-value-make-literal 7) (vm-value-make-literal 3))))
    (is (eq :int (vm-value-type a)))
    (is (= 21 (vm-value-payload a)))))

(test div-ints
  (let ((a (vm-value-div (vm-value-make-literal 7) (vm-value-make-literal 3))))
    (is (eq :int (vm-value-type a)))
    (is (= 2 (vm-value-payload a)))))

(test mod-ints
  (let ((a (vm-value-mod (vm-value-make-literal 7) (vm-value-make-literal 3))))
    (is (eq :int (vm-value-type a)))
    (is (= 1 (vm-value-payload a)))))

(test divide-mod-by-zero
  (let ((a (vm-value-make-literal 1))  (b (vm-value-make-literal 0)))
    (signals vm-divide-by-zero (vm-value-div a b))
    (signals vm-divide-by-zero (vm-value-mod a b))))

(test int-eq
  (let ((a (vm-value-make-literal 42)) (b (vm-value-make-literal 32)))
    (is (eq +vm-value-true+ (vm-value-eq a a)))
    (is (eq +vm-value-false+ (vm-value-eq a b)))))

(test string-eq
  (let* ((string-table (make-hash-table))
         (a (vm-value-make-literal "foo" :string-table string-table))
         (b (vm-value-make-literal "FOO" :string-table string-table)))
    (is (eq +vm-value-true+ (vm-value-eq a a)))
    (is (eq +vm-value-false+ (vm-value-eq a b)))))

(test bool-eq
  (is (eq +vm-value-true+ (vm-value-eq +vm-value-true+ +vm-value-true+)))
  (is (eq +vm-value-true+ (vm-value-eq +vm-value-false+ +vm-value-false+)))
  (is (eq +vm-value-false+ (vm-value-eq +vm-value-true+ +vm-value-false+))))

(test int-gt
  (let ((a (vm-value-make-literal 42))  (b (vm-value-make-literal 10)))
    (is (eq +vm-value-true+ (vm-value-gt a b)))
    (is (eq +vm-value-false+ (vm-value-gt b a)))
    (is (eq +vm-value-false+ (vm-value-gt a a)))))

(test string-gt
  (let* ((string-table (make-hash-table))
         (a (vm-value-make-literal "abc" :string-table string-table))
         (b (vm-value-make-literal "xyz" :string-table string-table)))
    (is (eq +vm-value-true+ (vm-value-gt b a)))
    (is (eq +vm-value-false+ (vm-value-gt a b)))
    (is (eq +vm-value-false+ (vm-value-gt a a)))))

(test bool-gt
  (is (eq +vm-value-true+ (vm-value-gt +vm-value-true+ +vm-value-false+)))
  (is (eq +vm-value-false+ (vm-value-gt +vm-value-false+ +vm-value-true+))))

(test int-lt
  (let ((a (vm-value-make-literal 42))  (b (vm-value-make-literal 10)))
    (is (eq +vm-value-false+ (vm-value-lt a b)))
    (is (eq +vm-value-true+ (vm-value-lt b a)))
    (is (eq +vm-value-false+ (vm-value-lt a a)))))

(test string-lt
  (let* ((string-table (make-hash-table))
         (a (vm-value-make-literal "abc" :string-table string-table))
         (b (vm-value-make-literal "xyz" :string-table string-table)))
    (is (eq +vm-value-false+ (vm-value-lt b a)))
    (is (eq +vm-value-true+ (vm-value-lt a b)))
    (is (eq +vm-value-false+ (vm-value-lt a a)))))

(test bool-lt
  (is (eq +vm-value-true+ (vm-value-lt +vm-value-false+ +vm-value-true+)))
  (is (eq +vm-value-false+ (vm-value-lt +vm-value-true+ +vm-value-false+))))

(test gt-needs-same-type
  (signals vm-type-error
    (vm-value-gt (vm-value-make-literal 10) (vm-value-make-literal 'false))))

(test none-comparison
  (is (eq +vm-value-true+ (vm-value-eq +vm-value-none+ +vm-value-none+)))
  (is (eq +vm-value-false+ (vm-value-lt +vm-value-none+ +vm-value-none+)))
  (is (eq +vm-value-false+ (vm-value-gt +vm-value-none+ +vm-value-none+))))

(test concat-strings
  (let* ((string-table (make-hash-table))
         (a (vm-value-make-literal "foo" :string-table string-table))
         (b (vm-value-make-literal "bar" :string-table string-table))
         (ab (vm-value-concat a b :string-table string-table)))
    (is (string= "foobar" (vm-value-payload ab)))))

(test concat-just-strings
  (let* ((string-table (make-hash-table))
         (a (vm-value-make-literal "foo" :string-table string-table))
         (b (vm-value-make-literal 10)))
    (signals vm-type-error (ab (vm-value-concat a b :string-table string-table)))))

(test substr-strings
  (let* ((string-table (make-hash-table))
         (a (vm-value-make-literal "foobar" :string-table string-table))
         (foo (vm-value-substr a
                               (vm-value-make-literal 0)
                               (vm-value-make-literal 3)
                               :string-table string-table))
         (bar (vm-value-substr a
                               (vm-value-make-literal 3)
                               +vm-value-none+
                               :string-table string-table)))
    (is (string= "foo" (vm-value-payload foo)))
    (is (string= "bar" (vm-value-payload bar)))))

(test strlen-strings
  (let* ((string-table (make-hash-table))
         (empty-string (vm-value-make-literal "" :string-table string-table))
         (a (vm-value-make-literal "12345" :string-table string-table)))
    (is (= 0 (vm-value-payload (vm-value-strlen empty-string))))
    (is (= 5 (vm-value-payload (vm-value-strlen a))))))

(test vec-constructor-and-rank
  (let* ((len 4)
         (vec (vm-value-make-array (list (vm-value-make-int len)))))
    (is (eq :array (vm-value-type vec)))
    (is (= 1 (vm-value-payload (vm-value-array-rank vec))))
    (is (= len (length (vm-value-payload vec))))))

(test vec-dim-size-and-oob
  (let* ((len 3)
         (vec (vm-value-make-array (list (vm-value-make-int len)))))
    (is (= len (vm-value-payload
                (vm-value-array-dim-size vec (vm-value-make-int 0)))))
    (signals vm-index-out-of-bounds
      (vm-value-array-dim-size vec (vm-value-make-int 1)))))

(test vec-set-get
  (let ((vec (vm-value-make-array (list (vm-value-make-int 2))))
        (zero (vm-value-make-int 0))
        (one (vm-value-make-int 1))
        (two (vm-value-make-int 2)))
    (vm-value-array-set vec (list zero) one)
    (vm-value-array-set vec (list one) two)
    (is (eq one (vm-value-array-get vec (list zero))))
    (is (eq two (vm-value-array-get vec (list one))))))

(test vec-set-get-oob
  (let ((vec (vm-value-make-array (list (vm-value-make-int 2))))
        (minus-one (vm-value-make-int -1))
        (two (vm-value-make-int 2)))
    (signals vm-index-out-of-bounds
      (vm-value-array-get vec (list minus-one)))
    (signals vm-index-out-of-bounds
      (vm-value-array-get vec (list two)))
    (signals vm-index-out-of-bounds
      (vm-value-array-set vec (list minus-one) +vm-value-none+))
    (signals vm-index-out-of-bounds
      (vm-value-array-set vec (list two) +vm-value-none+))))

(test vec-falsep
  (let* ((empty (vm-value-make-array (list (vm-value-make-int 0))))
         (non-empty (vm-value-make-array (list (vm-value-make-int 1)))))
    (is (vm-value-falsep empty))
    (is (not (vm-value-falsep non-empty)))))

(test vec-printer
  (let ((vec (vm-value-make-array (list (vm-value-make-int 2)))))
    (vm-value-array-set vec (list (vm-value-make-int 0)) (vm-value-make-int 10))
    (is (string= "[10, none]" (vm-value-str vec)))))

(test vec-empty-printer
  (let ((vec (vm-value-make-array (list (vm-value-make-int 0)))))
    (is (string= "[]" (vm-value-str vec)))))

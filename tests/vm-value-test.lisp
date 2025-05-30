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
  (let ((a (vm-value-make-literal "foo" :string-table (make-hash-table)))
        (b (vm-value-make-literal "foo" :string-table (make-hash-table))))
    (is (eq (vm-value-payload a) (vm-value-payload b)))))

(test not-does-its-thing
  (let ((a (vm-value-not (vm-value-make-literal 0))))
    (is (eq +vm-value-true+ a)))
  (let ((a (vm-value-not (vm-value-make-literal "" :string-table (make-hash-table)))))
    (is (eq +vm-value-true+ a)))
  (let ((a (vm-value-not +vm-value-false+)))
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
     (vm-value-make-literal "foo" (make-hash-table)))))

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

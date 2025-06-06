(in-package :vm1)

(defstruct compiler-context
  (reg-counter 0))

(defun fresh-reg (prefix ctx)
  (intern (format nil
                  (concatenate 'string prefix "~A")
                  (incf (compiler-context-reg-counter ctx)))))

(defun fresh-tmp (ctx)
  (fresh-reg "tmp" ctx))

(defun same-type (a b) (eq a b))

(defun resolve-operand (compiled)
  (or (getf compiled :literal) (getf compiled :reg)))

(defun %print (node ctx)
  (let* ((compiled-a (compile-node (cadr node) ctx))
         (a-operand (resolve-operand compiled-a))
         (a-code (getf compiled-a :code)))
    (list :literal 'none
          :type :none
          :code (append a-code
                        (list (list 'print a-operand))))))

(defmacro deflang-op (op-name instruction expected-types result-type)
  ;; op-name is e.g. 'substr
  ;; instruction is e.g. 'substr
  ;; expected-types is a list like (:string :int :int) or :any or :same-type
  ;; result-type is e.g. :string
  `((and (consp node) (eq ,op-name (car node)))
    (let ((args (cdr node)))
      (unless (= (length args) (length ,expected-types))
        (error 'compiler-error
               :message (format nil "wrong number of arguments to ~A: ~A" ',op-name args)
               :initarg node))
      (let ((compiled-args (mapcar (lambda (arg) (compile-node arg ctx)) args)))

        ;; Type check
        (let ((actual-types (mapcar (lambda (c) (getf c :type)) compiled-args)))
          (unless (or (equal '(:any) ,expected-types)
                      (and (equal '(:same-type) ,expected-types)
                           (= 1 (length (remove-duplicates ,expected-types))))
                      (equal ,expected-types actual-types))
            (error 'compiler-type-error :message (format nil "type mismatch for ~A" ,op-name)
                                        :node node
                                        :expected ,expected-types
                                        :actual actual-types))
          ;; Generate code
          (let ((tmp (fresh-tmp ctx))
                (code (apply #'append (mapcar (lambda (c) (getf c :code)) compiled-args)))
                (operands (mapcar #'resolve-operand compiled-args)))
            (list :reg tmp
                  :type ,result-type
                  :code (append code (list (cons ,instruction (cons tmp operands)))))))))))

(defmacro deflang-unop (op instruction expected-type result-type)
  `(deflang-op ,op ,instruction (list ,expected-type) ,result-type))

(defmacro deflang-binop (op instruction expected-type result-type)
  `(deflang-op ,op ,instruction (list ,expected-type ,expected-type) ,result-type))

(defmacro deflang-compile-node (&rest rules)
  `(defun compile-node (node ctx)
     (cond
       ,@(loop for rule in rules
               collect (macroexpand rule))
       (t (error 'compiler-error :message "unknown node" :node node)))))

(deflang-compile-node
    ((integerp node) `(:literal ,node :type :int))
    ((stringp node) `(:literal ,node :type :string))
  ((and (symbolp node) (or (eq 'false node) (eq 'true node)))
   `(:literal ,node :type :bool))

  (deflang-binop '+ 'add :int :int)
  (deflang-binop '- 'sub :int :int)
  (deflang-binop '* 'mul :int :int)
  (deflang-binop '/ 'div :int :int)
  (deflang-binop '% 'mod :int :int)
  (deflang-unop 'not 'not :any :bool)

  (deflang-binop 'concat 'concat :string :string)
  (deflang-op 'substr 'substr '(:string :int :int) :string)

  (deflang-binop '= 'eq :same-type :bool)
  (deflang-binop '< 'lt :same-type :bool)
  (deflang-binop '> 'gt :same-type :bool)

  ((and (consp node) (eq 'print (car node)))
   (%print node ctx)))

(defun compile-program (program)
  (let ((ctx (make-compiler-context)))
    (coerce
     (loop for node in program
           append (let ((compiled (compile-node node ctx)))
                    (getf compiled :code)))
     'vector)))

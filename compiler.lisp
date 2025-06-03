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

(defmacro deflang-unop (op instruction type-check resulting-type)
  `((and (consp node) (eq ,op (car node)))
    (let* ((compiled-a (compile-node (cadr node) ctx))
           (a-operand (resolve-operand compiled-a))
           (a-code (getf compiled-a :code))
           (a-type (getf compiled-a :type))

           (tmp (fresh-tmp ctx)))
      (unless (or (null ,type-check)
                  (if (functionp ,type-check)
                      (funcall ,type-check a-type)
                      (eq ,type-check a-type)))
        (error 'compiler-type-error :message (format nil "type mismatch for ~A" ,op)
                                    :node node
                                    :expected ,type-check
                                    :actual a-type))

      (list :reg tmp
            :type ,resulting-type
            :code (append a-code
                          (list (list ,instruction tmp a-operand)))))))

(defmacro deflang-binop (op instruction type-check resulting-type)
  `((and (consp node) (eq ,op (car node)))
    (destructuring-bind (a b) (cdr node)
      (let* ((compiled-a (compile-node a ctx))
             (a-operand (resolve-operand compiled-a))
             (a-code (getf compiled-a :code))
             (a-type (getf compiled-a :type))

             (compiled-b (compile-node b ctx))
             (b-operand (resolve-operand compiled-b))
             (b-code (getf compiled-b :code))
             (b-type (getf compiled-b :type))

             (tmp (fresh-tmp ctx)))
        (unless (or (null ,type-check)
                    (if (functionp ,type-check)
                        (funcall ,type-check a-type b-type)
                        (and (eq ,type-check a-type)
                             (eq ,type-check b-type))))
          (error 'compiler-type-error :message (format nil "type mismatch for ~A" ,op)
                                      :node node
                                      :expected ,type-check
                                      :actual (list a-type b-type)))

        (list :reg tmp
              :type ,resulting-type
              :code (append a-code b-code
                            (list (list ,instruction tmp a-operand b-operand))))))))

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
  (deflang-unop 'not 'not nil :bool)

  (deflang-binop '= 'eq #'same-type :bool)
  (deflang-binop '< 'lt #'same-type :bool)
  (deflang-binop '> 'gt #'same-type :bool)

  ((and (consp node) (eq 'print (car node)))
   (%print node ctx)))

(defun compile-program (program)
  (let ((ctx (make-compiler-context)))
    (coerce
     (loop for node in program
           append (let ((compiled (compile-node node ctx)))
                    (getf compiled :code)))
     'vector)))

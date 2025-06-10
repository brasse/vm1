(in-package :vm1)

(defstruct compiled-node
  kind           ; :literal, :reg, or :retvals
  types          ; list of declared types if available, nil otherwise
  code           ; emitted code vector
  reg            ; for kind = :reg
  literal        ; for kind = :literal
  ret-count)     ; for kind = :retvals

(defstruct compiler-context
  (reg-counter 0))

(defun fresh-reg (prefix ctx)
  (intern (format nil
                  (concatenate 'string prefix "~A")
                  (incf (compiler-context-reg-counter ctx)))))

(defun fresh-tmp (ctx)
  (fresh-reg "tmp" ctx))

(defun same-type (a b) (eq a b))

(defun primary-type (types) (car types))

(defun resolve-operand (compiled)
  (or (compiled-node-literal compiled)
      (compiled-node-reg compiled)))

(defun %print (node ctx)
  (let* ((compiled-a (compile-node (cadr node) ctx))
         (a-operand (resolve-operand compiled-a))
         (a-code (compiled-node-code compiled-a)))
    (make-compiled-node :kind :literal
                        :types '(:none)
                        :code (append a-code (list (list 'print a-operand))))))

(defmacro def-lang-op (op-name instruction expected-types result-type)
  ;; op-name is e.g. 'substr
  ;; instruction is e.g. 'substr
  ;; expected-types is a list like (:string :int :int) or :any or :same-type
  ;; result-type is e.g. :string
  `((and (consp node) (eq ,op-name (car node)))
    (let ((args (cdr node)))
      (unless (or (member ,expected-types '(:any :same-type))
                  (= (length args) (length ,expected-types)))
        (error 'compiler-error
               :message (format nil "wrong number of arguments to ~A, exptected ~A: ~A"
                                ',op-name (length ,expected-types) args)
               :node node))
      (let ((compiled-args (mapcar (lambda (arg) (compile-node arg ctx)) args)))

        ;; Type check
        (let ((actual-types (mapcar (lambda (c) (primary-type (compiled-node-types c))) compiled-args)))
          (unless (or (equal :any ,expected-types)
                      (and (equal :same-type ,expected-types)
                           (= 1 (length (remove-duplicates actual-types))))
                      (equal ,expected-types actual-types))
            (progn
              (error 'compiler-type-error :message (format nil "type mismatch for ~A" ,op-name)
                                          :node node
                                          :expected ,expected-types
                                          :actual actual-types)))
          ;; Generate code
          (let ((tmp (fresh-tmp ctx))
                (code (apply #'append (mapcar (lambda (c) (compiled-node-code c)) compiled-args)))
                (operands (mapcar #'resolve-operand compiled-args)))
            (make-compiled-node
             :kind :reg
             :reg tmp
             :types (list ,result-type)
             :code (append code (list (cons ,instruction (cons tmp operands)))))))))))

(defmacro def-lang-compile-node (&rest rules)
  `(defun compile-node (node ctx)
     (cond
       ,@(loop for rule in rules
               collect (macroexpand rule))
       (t (error 'compiler-error :message "unknown node" :node node)))))

(def-lang-compile-node
    ((integerp node) (make-compiled-node :kind :literal :types '(:int) :literal node))
    ((stringp node) (make-compiled-node :kind :literal :types '(:string) :literal node))
  ((and (symbolp node) (or (eq 'false node) (eq 'true node)))
   (make-compiled-node :kind :literal :types '(:bool) :literal node))

  (def-lang-op '+ 'add '(:int :int) :int)
  (def-lang-op '- 'sub '(:int :int) :int)
  (def-lang-op '* 'mul '(:int :int) :int)
  (def-lang-op '/ 'div '(:int :int) :int)
  (def-lang-op '% 'mod '(:int :int) :int)
  (def-lang-op 'not 'not :any :bool)

  (def-lang-op 'concat 'concat '(:string :string) :string)
  (def-lang-op 'substr-start-end 'substr-start-end '(:string :int :int) :string)
  (def-lang-op 'substr-start 'substr-start '(:string :int) :string)
  (def-lang-op 'strlen 'strlen '(:string) :int)

  (def-lang-op '= 'eq :same-type :bool)
  (def-lang-op '< 'lt :same-type :bool)
  (def-lang-op '> 'gt :same-type :bool)

  ((and (consp node) (eq 'print (car node)))
   (%print node ctx)))

(defun compile-program (program)
  (let ((ctx (make-compiler-context)))
    (coerce
     (loop for node in program
           append (let ((compiled (compile-node node ctx)))
                    (compiled-node-code compiled)))
     'vector)))

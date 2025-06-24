(in-package :vm1)

(defparameter +factorial-asm-iterative+
  #((const n 8)
    (call factorial n)
    (get-ret result 0)
    (print result)
    (halt)
    (label factorial)
    (get-arg n 0)
    (const acc 1)
    (label loop)
    (jz done n)
    (mul acc acc n)
    (sub n n 1)
    (jmp loop)
    (label done)
    (ret acc)))

(defparameter +factorial-asm-recursive+
  #((const n 8)
    (call factorial n)
    (get-ret result 0)
    (print result)
    (halt)

    (label factorial)
    (get-arg n 0)

    (eq tst n 1)                ; return 1 if n == 1
    (jz recur tst)
    (ret 1)

    (label recur)

    (mov a n)                   ; fact(n - 1)
    (sub a a 1)
    (call factorial a)
    (get-ret fact-0 0)

    (mul result fact-0 n)       ; fact(n) = fact(n - 1) * n
    (ret result)))

(defparameter +factorial-asm-tail-recursive+
  #((const n 8)
    (const acc 1)
    (call factorial n acc)
    (get-ret result 0)
    (print result)
    (halt)

    (label factorial)
    (get-arg n 0)
    (get-arg acc 1)

    (eq tst n 1)                ; return 1 if n == 1
    (jz recur tst)
    (ret acc)

    (label recur)

    (mul acc acc n)             ; acc = acc * n
    (sub n n 1)                 ; n = n - 1
    (tail-call factorial n acc)
    (get-ret result 0)

    (ret result)))

(defparameter +fibonacci-asm-iterative+
  #((const n 20)
    (call fibonacci n)
    (get-ret result 0)
    (print result)
    (halt)
    (label fibonacci)
    (get-arg n 0)
    (const a 0)
    (const b 1)
    (label loop)
    (jz done n)
    (add tmp a b)
    (mov a b)
    (mov b tmp)
    (sub n n 1)
    (jmp loop)
    (label done)
    (ret a)))

(defparameter +fibonacci-asm-recurursive+
  #((const n 20)
    (call fibonacci n)
    (get-ret result 0)
    (print result)
    (halt)

    (label fibonacci)
    (get-arg n 0)

    (gt tst n 2)                ; return 1 if n <= 2
    (jnz recur tst)
    (ret 1)

    (label recur)

    (mov a n)                   ; fib(n - 1)
    (sub a a 1)
    (call fibonacci a)
    (get-ret fib-1 0)

    (mov b n)                   ; fib(n - 2)
    (sub b b 2)
    (call fibonacci b)
    (get-ret fib-2 0)

    (add result fib-1 fib-2)    ; fib(n) = fib(n - 1) + fib(n - 2)
    (ret result)))

(defparameter +fibonacci-asm-tail-recurursive+
  #((const n 20)
    (const a 1)
    (const b 1)
    (call fibonacci n a b)
    (get-ret result 0)
    (print result)
    (halt)

    (label fibonacci)
    (get-arg n 0)
    (get-arg a 1)
    (get-arg b 2)

    (eq tst 1 n)                 ; return a if n = 1
    (jz recur tst)
    (ret a)

    (label recur)

    (sub n n 1)
    (add a a b)

    (tail-call fibonacci n b a)      ; fib(n - 1, b, (a + b))
    (get-ret result 0)

    (ret result)))

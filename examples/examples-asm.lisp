(in-package :vm1)

(defparameter +factorial-asm+
  #((const n 5)
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

(defparameter +fibonacci-asm-iter+
  #((const n 6)
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

(defparameter +fibonacci-asm-recur+
  #((const n 6)
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

    (mov a n)                   ; get fib(n - 1)
    (sub a a 1)
    (call fibonacci a)
    (get-ret fib-1 0)

    (mov b n)                   ; get fib(n - 2)
    (sub b b 2)
    (call fibonacci b)
    (get-ret fib-2 0)

    (add result fib-1 fib-2)    ; fib(n) = fib(n - 1) + fib(n - 2)
    (ret result)))

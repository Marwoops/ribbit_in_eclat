(define fibo (lambda (n)
	(if (= n 0)
	  0
	  (if (= n 1)
		1
		(+ (fibo (- n 1)) (fibo (- n 2)))))))

(fibo 10)

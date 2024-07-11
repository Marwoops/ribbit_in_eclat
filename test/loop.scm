(define loop
  (lambda (n)
	(if (= n 0)
	  0
	  (if (= n 1)
		1
		(loop (- n 2))))))
(loop (loop 5))

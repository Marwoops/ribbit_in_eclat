(define print_bool
  (lambda (b)
	(if b
	  (putchar (+ 65 20))
	  (putchar (+ 65 6)))
	(putchar 10)))

(print_bool (< 0 1))
(print_bool (< 1 1))

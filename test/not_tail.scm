(define addone
  (lambda (n)
	(+ n 1)))
(define t
  (lambda (a b)
	(+ (addone a) (addone b))))
(t 1 2)

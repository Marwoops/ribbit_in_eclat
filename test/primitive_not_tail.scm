(define plustwo
  (lambda (n)
	(+ (- (- n 2) 2) 6)))
(plustwo (plustwo 10))

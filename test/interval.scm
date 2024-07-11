(define interval
  (lambda (a b)
	(if (> a b)
	  nil
	  (cons a (interval (+ a 1) b)))))

(interval 1 10)

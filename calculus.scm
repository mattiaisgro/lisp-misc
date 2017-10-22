
(define dx 0.0001)
(define integral-precision 1000.0)

;;; Calculus approximations and other math functions

;;; Return a function that approximates  the derivative of f(x)
(define (derive-f f)
	(lambda (x)
		(/	(abs (- (f (+ x *dx*)) (f x)))
			*dx*)
	)
)

;;; Return a function that approximates the integral of f(x)
;;; Using Cavalieri-Simpson approximation
(define (integrate-f f)

	(define (yk-iter f a b counter res)
		(if (> counter *integral-precision*)
			res
			(yk-iter f a b (1+ counter)
				(+ res
					(f (+ a
						(* counter
							(/ 	(distance a b)
								integral-precision)))))) ; f(a + kh)
		)
	)

	(lambda (a b)
		(* 	(/ (distance a b) integral-precision) ; h
			(yk-iter f a b 0.0 0.0))) ; sum of Yk
)



;;; Return a function that calculates the derivative of ax^n
;;; ax^n => (a * n) * x^(n - 1)
(define (derive-x-monomial coeff power)
	(lambda (x) (* 	coeff
					power
					(pow x (1- power))))
)

;;; Return a function that calculates the integral of ax^n
;;; ax^n => (a * x^(n + 1)) / (n + 1)
(define (integrate-x-monomial coeff power)
	(lambda (x) (/ 	(* coeff (pow x (1+ power)))
					(1+ power)))
)

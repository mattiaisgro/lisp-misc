

; Constants
(defparameter *precision* 10000.0)
(defparameter *dx* 0.00001)
(defparameter *integral-precision* 2000.0)


; Functions

(defun square (x) (* x x))

(defun cube (x) (* x x x))

(defun divides-by? (x y)
	(if (zerop (mod x y))
		t
		nil)
)

; Execute a procedure with some probability, else execute (other)
(defun call-probability (f other probability)
	(if (< random probability)
		(funcall f)
		(funcall other))
)

(defun average (x y)
	(/ (+ x y) 2.0)
)

(defun weighed-average (x xweight y yweight)
	(/	(+ (* x xweight) (* y yweight))
		(+ xweight yweight))
)

(defun distance (x y)
	(abs (- x y)))

(defun distance-2d (x1 y1 x2 y2)
	(sqrt (+ (* x1 y1) (x2 y2))))

; Calculate x^y (y must be natural)
(defun pow (x y)
	(defun pow-iter (res x y counter)
		(if (= counter y)
			res
			(pow-iter (* res x) x y (1+ counter)))
	)
	(defun pow-iter-inverse (res x y counter)
		(if (= counter y)
			res
			(pow-iter-inverse (/ res x) x y (1+ counter)))
	)
	(if (>= y 0)
		(pow-iter 1.0 x y 0)
		(pow-iter-inverse 1.0 x (abs y) 0))
)

; Return a function that approximates f'(x)
(defun derive-f (f)
	(lambda (x)
		(/	(abs (- (funcall f (+ x *dx*)) (funcall f x)))
			*dx*)
	)
)

(defun serie-sum (f a f-next b)
	(if (> a b)
		0
		(+ 	(funcall f a)
			(serie-sum f (funcall f-next a) f-next b)))
)


; Return a function that approximates the integral of f(x)
; Using Cavalieri-Simpson approximation
(defun integrate-f (f)

	(defun yk-iter (f a b counter res)
		(if (> counter *integral-precision*)
			res
			(yk-iter f a b (1+ counter)
				(+ res
					(funcall f (+ a
						(* counter
							(/ 	(distance a b)
								*integral-precision*)))))) ; f(a + kh)
		)
	)

	(lambda (a b)
		(* 	(/ (distance a b) *integral-precision*); h
			(yk-iter f a b 0.0 0.0))) ; sum of Yk
)

; (defun horner-polynomial (items x)
; 	(defun horner-iter (items x count)
; 		(if (= items nil)
; 			0
; 			(+ 	(* (car items) (pow x count))
; 				(horner-iter (cdr items) x (+ 1 count))))
; 	)

; 	(horner-iter items x 0)
; )


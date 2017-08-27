

; Constants
(defvar *precision* 10000000.0)
(defvar *dx* (/ 1.0 *precision*))


; Functions

(defun square (x) (* x x))

(defun cube (x) (* x x x))

; Calculate x^y (y must be natural, use fpow for decimals)
(defun pow (x y)
	(defun pow-iter (res x y counter)
		(if (= counter y)
			res
			(pow-iter (* res x) x y (+ counter 1)))
	)
	(defun pow-iter-inverse (res x y counter)
		(if (= counter y)
			res
			(pow-iter-inverse (/ res x) x y (+ counter 1)))
	)
	(if (>= y 0)
		(pow-iter 1.0 x y 0)
		(pow-iter-inverse 1.0 x (abs y) 0))
)

; Return a function that approximates f'(x)
(defun derive-f (f)
	(lambda (x)
		(/
			(abs (- (funcall f (+ x *dx*)) (funcall f x)))
			*dx*)))

; Return a function that approximates the integral of f(x)
; Using Simpson approximation
(defun integrate-f (f)
	(defun integ-simpson (f a b k res)
		(if (= k *integral-precision*)
			res
			(+ res
				(funcall f (+ a (* k (/ (- b a) *integral-precision*))))
			)))

	(lambda (a b)
		(*
			(/ (- b a) *integral-precision*)
			(integ-simpson f a b (/ *precision* 100.0) 0.0))))


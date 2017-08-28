

; Constants
(defparameter *precision* 10000.0)
(defparameter *dx* 0.00001)
(defparameter *integral-precision* 1000.0)


; Functions

(defun square (x) (* x x))

(defun cube (x) (* x x x))

(defun divides-by? (x y)
	(if (zerop (mod x y))
		t
		nil))

; Execute a procedure with some probability, else execute (other)
(defun call-probability (f other probability)
	(if (< random probability)
		(funcall f)
		(funcall other)))

(defun average (x y)
	(/ (+ x y) 2.0))

(defun weighed-average (x xweight y yweight)
	(/	(+ (* x xweight) (* y yweight))
		(+ xweight yweight)))


; Calculate x^y (y must be natural)
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
; Using Cavalieri-Simpson approximation
(defun integrate-f (f)
	(defun integ-simpson (f a b k res)
		(if (= k *integral-precision*)
			res
			(+ res
				(funcall f (+ a (* k (/ (- b a) *integral-precision*))))
			)))

	(lambda (a b)
		(*
			(/ (/ (- b a) 3.0) *integral-precision*)
			(integ-simpson f a b (/ *precision* 100.0) 0.0))))


; (defun integrate-f-other (f)
; 	(defun integ-iter (f a b counter)
; 		())

; 	(lambda (a b) (integ-iter f a b counter)))


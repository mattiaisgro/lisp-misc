
;;; Basic math functions


;;; Return the square of (x)
(define (square x) (* x x))

;;; Return the cube of (x)
(define (cube x) (* x x x))

;;; Check if x is divisible by y
(define (divides-by? x y)
	(if (= (mod x y) 0)
		#t
		#f)
)

;;; Calculate the absolute difference between two numbers
(define (distance x y)
	(abs (- x y))
)

;;; Calculate the distance between two points in 2D space
(define (distance-2d x1 y1 x2 y2)
	(sqrt (+ (square (- x1 x2)) (square (- y1 y2))))
)

;;; Calculate the distance between two points in 3D space
(define (distance-3d x1 y1 z1 x2 y2 z2)
	(sqrt (+ (square (- x1 x2)) (square (- y1 y2)) (square (- z1 z2))))
)

;;; Check if two numbers are close enough (for approximations)
(define (good-enough? x y)
	(if (< (distance x y) dx)
		#t
		#f)
)

;;; Sum numbers based on passed functions
;;; (f) operates on current number
;;; (f-next) returns next number
(define (serie-sum f a f-next b)
	(if (> a b)
		0
		(+ 	(f a)
			(serie-sum f (f-next a) f-next b)))
)

;;; Sum all numbers in a list
(define (sum-all l)
	(define (sum-all-iter l res counter)
		(if (null? (cdr l))
			(+ res (car l))
			(sum-all-iter (cdr l) (+ res (car l)) (1+ counter)))
	)

	(sum-all-iter l 0.0 0)
)

;;; Calculate the average of two numbers
(define (average x y)
	(/ (+ x y) 2.0)
)

;;; Calculate the average of a list of numbers
(define (average-list l)
	(/ (sum-all l) (list-lenght l))
)

;;; Calculate the weighed average of two numbers
(define (weighed-average x xweight y yweight)
	(/	(+ (* x xweight) (* y yweight))
		(+ xweight yweight))
)

;;; Adjust (x) in (a) (b) range
(define (clamp x a b)
	(if (> x b)
		b
		(if (< x a)
			a
			x))
)

;;; Calculate the factorial of (x)
(define (factorial x)
	(define (factorial-iter x res)
		(if (or (= x 1) (zero? x))
			res
			(factorial-iter (1- x)
							(* res x)))
	)

	(factorial-iter x 1.0)
)

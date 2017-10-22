
;;; Work on math functions

;;; Return a function that calculates f(g(x))
(define (compose-f f g)
	(lambda (x) (f (g x)))
)

;;; Return a function that calculates f(x) + g(x)
(define (add-f f g)
	(lambda (x) (+ (f x) (g x)))
)

;;; Return a function that calculates f(x) * g(x)
(define (mul-f f g)
	(lambda (x) (* (f x) (g x)))
)

;;; Return a function that calculates f(x) - g(x)
(define (sub-f f g)
	(lambda (x) (- (f x) g(x)))
)

;;; Return a function that calculates f(x) / g(x)
(define (div-f f g)
	(lambda (x) (/ (f x) (g x)))
)

;;; Return a function that returns a pure number
(define (purenum k)
	(lambda (x) (+ 0 k))
)

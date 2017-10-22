
;;; Interpolation functions


;;; Linear interpolation
(define (lerp a b coeff)
	(+ (* a (- 1 coeff)) (* b coeff))
)


;;; Smoothstep interpolation
(define (smoothstep a b coeff)
	(define (smoothstep-res result)
		(* result result (- 3 (* 2 result))))

	(smoothstep-res (clamp (/ (- coeff a) (- b a)) 0 1))
)


;;; Smootherstep interpolation
(define (smootherstep a b coeff)
	(define (smootherstep-res result)
		(* result result result (+ (* result (- (* result 6) 15)) 10)))

	(smootherstep-res (clamp (/ (- coeff a) (- b a)) 0 1))
)

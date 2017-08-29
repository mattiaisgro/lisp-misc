
;;; Approximations for most common constants

;;; PI

;;; Approximate PI using Ramanujan serie
(define (approx-pi-ramanujan n)
	(define (ramanujan-serie n k res)
		(if (= n k)
			res
			(ramanujan-serie n (1+ k)
				(+ res
					(/ 	(* (factorial (* 4 k)) (+ 1103 (* 26390 k)))
						(* (expt (factorial k) 4) (expt 396 (* 4 k)))))))
	)

	(/ 1.0 (* 	(/ (* 2 (sqrt 2)) 9801.0)
		(ramanujan-serie n 0 0)))
)

;;; Approximate PI using Vega formula (PI / 4 = 5 * arctan(1 / 5) - arctan(1 / 239))
(define (approx-pi-vega)
	(* 	4.0
		(- 	(* 4 (atan (/ 1.0 5.0)))
			(/ 1.0 239.0)))
)

;;; Approximate PI using Euler formula (20 * arctan(1 / 7) + 8 * arctan(3 / 79))
(define (approx-pi-euler)
	(+ 	(* 20 (atan (/ 1.0 7.0)))
		(* 8 (atan (/ 3.0 79.0))))
)


;;; PHI

;;; TO-DO


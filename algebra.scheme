
;;; Return a function that evaluates the equation expressed by (coeff-list)
;;; (list 1 3 0 6) => 1 + 3x + 6x^3
(define (horner-polynomial coeff-list)

	;;; Pass eq-lenght to calculate lenght of list only once
	(define (horner-iter coeff-list eq-lenght x counter res)
		(if (> counter eq-lenght)
			res
			(horner-iter coeff-list eq-lenght x (1+ counter)
				(+ res (* 	(list-ref coeff-list counter)
							(expt x counter)))))
	)

	(lambda (x) (horner-iter coeff-list (list-lenght coeff-list) x 0 0))
)

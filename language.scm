
; Place in function definitions to signal a TO-DO
(define (to-do)
	(display "TO-DO")
	(newline)
	(display "This function has currently no implementation"))

; Return the lenght of a list
(define (list-lenght l)
	(define (list-lenght-iter l counter)
		(if (null? (cdr l))
			counter
			(list-lenght-iter (cdr l) (1+ counter)))
	)

	(list-lenght-iter l 0)
)

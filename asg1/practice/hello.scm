#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
#lang racket
;The first program

(provide addMe factorial)


(define addMe (lambda (x y) (+ x y)) )

(define factorial (lambda (num) 
	(if (eq? 1 num) 
		1 ;test comment
		(* num (factorial (- num 1)) )
	)
))


(define printList (lambda (theList index)
	(if(eq? index 0)
		(display (list-ref theList 0) )
		(begin
			(display (list-ref theList index) )
			(newline)
			(printList theList (- index 1) )
		)
	)
))

(define aList '(1 2 3 4 5 6 7))
; (printList aList 6)

; (define fib (lambda (n)
; 	(define fibb (lambda (n a b)
; 		(if (< n 1)
; 			a
; 			(fibb (- n 1) b (+ a b) )
; 		)

; 	))
; ))

(define (f n)
   (define (f* n a b)
      (if (< n 1) a
          (f* (- n 1) b (+ a b))))
   (f* n 0 1))



(define (grep f l) 
	(map (lambda (x) (when (f x) x)) l)
)


(define (foldl f o l) 
	(if (null? l)
		o
		(foldl f (f o (car l)) (cdr l)) 
	)
)



; (define (merge list1 list2)
; 	(if (null? list1)
; 		list2
; 		(if (null? list2)
; 			list1
; 			(if (< (car list1) (car list2)) 
; 				(append (car list1) (merge (cdr list1) list2) )
; 				(append (car list2) (merge list1 (cdr list2) ) )
; 			)
; 		)
; 	) 
; )


(define (merge list1 list2)
	(if (null? list1)
		list2 
		(if (null? list2) 
			list1
			(if (< (car list1) (car list2)) 
				(cons (car list1) (merge (cdr list1) list2))
				(cons (car list2) (merge list1 (cdr list2) ))
			)
		)
	)
)


(define (contains f l)
	(if (null? l) false
		(if (f (car l)) true
			(contains f (cdr l) )
		)
	)
)

(define x 
	(contains (lambda (x) (= x 3)) '(1 2 4) )
)

(display x)
(newline)
; (newline)
; (display "______________________________")
; (newline)


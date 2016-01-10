#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

(require racket)
(require "sbi.scm")

;;Keeping track of the tests that pass or fail:
(define passedLog 0)
(define errorLog 0)

;;Custom exception
(define-struct unitTestException (errorMessage))

;;Exception handler:
(define (myHandler exn) (display (string-append "ERROR: "
                        (unitTestException-errorMessage
                         exn)))
  (set! errorLog (add1 errorLog))
)

;;A General assert function, compares two things, complains if they 
;;are not equal:
(define (assert name functionOutput shouldEqual)
  (with-handlers ([unitTestException? myHandler])
         (if (eqv? functionOutput shouldEqual)
             ((lambda () (display (string-append name " Passes"))
                 (set! passedLog (add1 passedLog))))
             (raise (make-unitTestException (string-append
                             name " failed")))
             )
         )
  (newline))

(newline)
(display "_______Unit Test______")
(newline)
;;Actual Tests to run:
;;note the function used, is exported from simple.scm

(assert "(print_imp null)" (print_imp  null) -1)
;(assert "(print_imp (string-split 'Hello World)'))" (print_imp  (string-split "Hello World)")) "HelloWorld")

;(display (print_imp  (string-split "Hello World)")) )

;(display (clean_brakets_spotless  "(((Pollo Negro))") )

(assert "(add_imp null)" (add_imp  null) -1)
(assert "(add_imp  (string-split '1 56)'))" (add_imp  (string-split "1 56)")) 57.0)
(assert "(add_imp  (string-split '56))'))" (add_imp  (string-split "56))")) 56.0)
;(assert "(add_imp  (string-split '1 10 56)'))" (add_imp  (string-split "1 10 56)")) 67.0)

(assert "(sub_imp null)" (sub_imp  null) -111)
(assert "(sub_imp  (string-split '2 56)'))" (sub_imp  (string-split "2 56)")) -54.0)
(assert "(sub_imp  (string-split '56))'))" (sub_imp  (string-split "56))")) -56.0)
(begin (display (sub_imp  (string-split "56))")) ))
(assert "(sub_imp  (string-split '56 9)'))" (sub_imp  (string-split "56 9)")) 47.0)
;(assert "(sub_imp  (string-split '56 9 1)'))" (sub_imp  (string-split "56 9 1)")) 46.0)

(assert "(mult_imp null)" (mult_imp  null) -1)
(assert "(mult_imp  (string-split '2 56)'))" (mult_imp  (string-split "2 56)")) 112.0)
(assert "(mult_imp  (string-split '2 9 56)'))" (mult_imp  (string-split "2 9 56)")) 1008.0)

(assert "(div_imp null)" (div_imp  null) -1)
(assert "(div_imp  (string-split '4 0)'))" (div_imp  (string-split "4 0)")) '+inf.0)
(assert "(div_imp  (string-split '1 4)'))" (div_imp  (string-split "1 4)")) .25)

(assert "(mod_imp null)" (mod_imp  null) -1)
(assert "(mod_imp  (string-split '2 56)'))" (mod_imp  (string-split "2 56)")) 2.0)
(assert "(mod_imp  (string-split '57 2)'))" (mod_imp  (string-split "57 2)")) 1.0)

(assert "(power_imp null)" (power_imp  null) -1)
(assert "(power_imp  (string-split '2 3)'))" (power_imp  (string-split "2 3)")) 8.0)
(assert "(power_imp  (string-split '10 5)'))" (power_imp  (string-split "10 5)")) 100000.0)


(assert "(let_imp null)" (let_imp  null) -111)
(let_imp  (string-split "pollo 3)"))
(assert "(let_imp (pollo 4))" (return_value "pollo") 3)

(assert "(dim_imp null)" (dim_imp  null) -1)
(dim_imp  (string-split "(arr 4)"))
(let_imp (string-split "(arr 2) 9))"))
(let_imp (string-split "(arr 1) (+ 2 7)))"))
(assert "(array_ref 'arr(2)')" (array_ref  (string-split "arr (2)")) 9.0)
(assert "(array_ref 'arr(1)')" (array_ref  (string-split "arr (1)")) 9.0)
(assert "(array_ref 'arr(pollo)')" (array_ref  (string-split "arr(pollo)" "(")) 0)
(display  (string-split "arr(pollo)" "(") )

;(display (add_imp  (string-split "(* 3 4) (* 5 6))")) (newline) )
;(assert "input_imp  1 6" (input_imp  1 6) 7)

; (assert "dim_imp null" (dim_imp  null) "pollo")
; (assert "dim_imp 'pollo" (dim_imp  "pollo") "pollo")

; (assert "let_imp null" (let_imp  null) "pollo")
; (assert "let_imp 'pollo" (let_imp  "pollo") "pollo")

; (assert "if_imp null" (if_imp  null) "pollo")
; (assert "if_imp 'pollo" (if_imp  "pollo") "pollo")

; (assert "goto_imp null" (goto_imp  null) "pollo")
; (assert "goto_imp 'pollo" (goto_imp  "pollo") "pollo")







;;Summary after all tests are run:
(newline)
(display (string-append "Number of Failures: " (number->string
                        errorLog)))
(newline)
(display (string-append "Number of Passes: " (number->string passedLog)))
(newline)
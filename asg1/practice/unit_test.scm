#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
(require "hello.scm")

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
         (if (eq? functionOutput shouldEqual)
             ((lambda () (display (string-append name " Passes"))
                 (set! passedLog (add1 passedLog))))
             (raise (make-unitTestException (string-append
                             name " failed")))
             )
         )
  (newline))


;;Actual Tests to run:
;;note the function used, is exported from simple.scm
(assert "factorial 1" (factorial 1) 1)
(assert "factorial 3" (factorial 3) 6)
(assert "factorial 7" (factorial 7) 5040)


;;Summary after all tests are run:
(newline)
(display (string-append "Number of Failures: " (number->string
                        errorLog)))
(newline)
(display (string-append "Number of Passes: " (number->string passedLog)))
(newline)
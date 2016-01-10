#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; Alejandro Aguilar aaguil10 1276359
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an
;;    SBIR program, which is the executed. 
;;

(require racket)
; #lang racket 
; (provide print_imp input_imp dim_imp let_imp if_imp goto_imp 
;   clean_brakets clean_brakets_spotless add_imp sub_imp mult_imp
;   div_imp mod_imp power_imp array_ref dim_imp return_value)


;Name: ok?
;Purpose: check for null
;inputs: x - thing that you want to check
;outputs: true or false
;side effects: none
;Recursive: No
(define (ok? x) (not (null? x)))

; (define (num_ok? x) (if (ok? x)
;   (number? x)
;   (ok? x)
; ))


;Name: print_imp
;Purpose: to execute print statement
;inputs: a - value that you want to print
;outputs: none
;side effects: will print "a" to standard output
;Recursive: Yes
(define print_imp (lambda (a)
  (if (ok? a)
    (if (string=? (substring (car a) 0 1) "(")
      (if (vector? (return_value (car a))) 
        (begin (display (array_ref a)) (newline))
        ;recursive statement to do other computation
        (begin (display (statement_parser a) ) )
      )
      (if (string=? (substring (car a) 
            (- (string-length (car a)) 1) 
            ;base case is the first string with )
            (string-length (car a))) ")") 
          (begin (display (return_value (car a)) ) (newline) )
          (if (vector? (return_value (car a)))
            (begin (display (array_ref a)) (newline) )
            (begin 
              (display (string-append (~a (return_value (car a))) " ")) 
              (print_imp (cdr a)) 
              (newline)  
            )  ;recursive statement to print next word
          )
        ) 
    )
    -1 
  )
))


;Name: input_imp
;Purpose: to insert input variables into the symbol table
;inputs: a - vector with arguments
;outputs: -1 when we reach the end of file
;side effects: prints error message if anything that is not a number
; is read in.
;Recursive: No
(define input_imp (lambda (a) 
  (readlist-from-inputfile (car a)) 
))

;Name: dim_imp
;Purpose: creates arrays where elements are intilized to 0
;inputs: a - vector with arguments
;outputs: none
;side effects: stated above
;Recursive: No
(define dim_imp (lambda (a) 
  (if (ok? a)
    (if (string=? (substring (cadr a) 0 1) "(")
      ;compute expression before adding to table
      (hash-set! *variable-table*   
        (clean_brakets_spotless (car a))
        (make-vector (statement_parser (cdr a)) 0)
      )
      (hash-set! *variable-table*  ;just add value to table
        (clean_brakets_spotless (car a)) 
        (make-vector 
          (string->number(clean_brakets_spotless (cadr a))) 
          0
        )
      )
    )
    -1 
  )
))

;Name: array_ref
;Purpose: Helper function to help refrence an element in an array
;inputs: a - vector with arguments, car is name of array
;cadr is the index
;outputs: the value of the array
;side effects: none
;Recursive: No
(define array_ref (lambda (a) 
  (if (ok? a)
    (if (vector? (return_value (car a) )) 
      (vector-ref 
        (hash-ref *variable-table* (clean_brakets_spotless (car a) ))
        (return_value (cadr a)) 
      )
      (if (vector? (return_value (cadr (string-split (car a) "(")) ))
        (vector-ref 
          (hash-ref *variable-table* 
            (return_value (cadr (string-split (car a) "(")) )
          )
          (return_value (cadr a)) 
        )
        -111
      )
    )
    -111
  )
))

;Name: array_set
;Purpose: Sets the values of a particular element in the array.
;inputs: a - vector with arguments
;outputs: none
;side effects: stated above 
;Recursive: No
(define array_set (lambda (a)
  (if (ok? a)
    (if (vector? (return_value (car a) ))
      (if (string=? (substring (caddr a) 0 1) "(") 
        (vector-set! 
          (hash-ref *variable-table* (clean_brakets_spotless (car a)))
          (return_value (cadr a))
          (statement_parser (cddr a))
        )
        (vector-set! 
          (hash-ref *variable-table* (clean_brakets_spotless (car a)))
          (return_value (cadr a)) 
          (+ (return_value (caddr a)) 0.0)
        )
      )
      -111
    )
    -111
  )
))

;Name: let_imp
;Purpose: Insert variable in variable table
;inputs: a - vector with arguments
;outputs: none
;side effects: stated above
;Recursive: Yes
(define let_imp (lambda (a) 
  (if (ok? a)
    (if (vector? (return_value (car a)))
      (array_set a) ;base case
      (if (string=? (substring (cadr a) 0 1) "(")
        (hash-set! *variable-table*   
          (clean_brakets_spotless (car a)) 
          (statement_parser (cdr a)) ;recursive part
        )
        (hash-set! *variable-table* ;base case  
          (clean_brakets_spotless (car a)) 
          (string->number(clean_brakets_spotless (cadr a))) 
        )
      ) 
    )
    -111
  )
))

;Name: eval_if
;Purpose: To evatuate a conparison
;inputs: relop, and two values to compare
;outputs: #f #t
;side effects: none
;Recursive: No
(define eval_if (lambda (relop val1 val2) 
  (case relop
    ( ("=") (= val1 val2))
    ( (">") (> val1 val2))
    ( ("<") (< val1 val2))
    ( ("<>") (not (= val1 val2)) )
    ( ("<=") (<= val1 val2))
    ( (">=") (>= val1 val2))
    (else #f)
  )
))

;Name: if_imp
;Purpose: evaluate print statements
;inputs: a - vector with arguments
;outputs: none
;side effects: will triger a change in control flow is comarison is 
;evaluated to true
;Recursive: Yes
(define if_imp (lambda (a) 
  (if (ok? a)
  (if (eval_if (clean_brakets_spotless (car a)) ;base case 
      (return_value (cadr a))
      (return_value (caddr a))
      )
    (if (hash-has-key? *lable-table* 
        (clean_brakets_spotless  (cadddr a)))
      (string-append 
        "if_stop " ;triggers recusive statement in control flow
        (~a (hash-ref *lable-table* 
          (clean_brakets_spotless (cadddr a) )))
      )
      -111
    )
    -111 
  )
  -111
  )
))

;Name: goto_imp
;Purpose: changes control flow to goto the lable
;inputs: a - vector with arguments
;outputs: none
;side effects: stated above 
;Recursive: Yes
(define goto_imp (lambda (a) 
  (if (ok? a)
    (if (hash-has-key? *lable-table* (clean_brakets_spotless  (car a)))
      (string-append 
        "goto_stop " ;triggers control flow statement in control_flow
        (~a (hash-ref *lable-table* (clean_brakets_spotless (car a) )))
      )
      -111 ;base case
    )
    -111
  )
))

;Name: return_value
;Purpose: checks and returns values that are variable table
;inputs: a - vector with arguments
;outputs: real number or string if nothing found
;side effects: none
;Recursive: No
(define return_value (lambda (item)
  (if (number? (string->number  (clean_brakets_spotless  item)) )
    (string->number (clean_brakets_spotless item))
    (if (hash-has-key? *variable-table* (clean_brakets_spotless  item))
      (hash-ref *variable-table* (clean_brakets_spotless item ))
      (clean_brakets_spotless item)
    )
  )
))

;Name: print_list
;Purpose: help function to see what is being passes as an arguement
;inputs: 1 - vector with arguments
;outputs: none
;side effects: print list to consel
;Recursive: Yes
(define print_list (lambda (l)
  (if (= (length l) 1)
    ;base case
    (display (car l)) 
    ;recursive case
    (begin (display (car l) ) (display " - ") (print_list (cdr l)) ) 
  )
))

;Name: expression_logic
;Purpose: responsible for all arithmetic functions
;inputs: f - arithmetic function, a - arguements for function 
;outputs: value computed by function
;side effects: none
;Recursive: Yes
(define expression_logic (lambda (f a) 
  (if (ok? a)
    (if (string=? (substring (car a) 0 1) "(")
      (if (string=? (substring (cadr a) 0 1) "(")
        ;recursive statement to do other computation
        (f (statement_parser  a) (statement_parser (cadr a)) )
         (statement_parser  a) 
      )
      (if (string=? (substring (car a) 
          (- (string-length (car a)) 1) 
          (string-length (car a))) ")") 
          ;base case is the first string with )
        (+ (return_value (car a)) 0.0) 
        ;recursive statement to print next word
        (f (return_value (car a) ) (expression_logic f (cdr a)) )  
      )
    )
    -1 
  )
))

;see expression_logic
(define add_imp (lambda (a) 
  (expression_logic + a)
))

;see expression_logic
(define sub_imp (lambda (a) 
  (if (ok? a) 
    (if (string=? (substring (car a) 
            (- (string-length (car a)) 1) 
            (string-length (car a))) ")")
    (-  0.0 (return_value(car a)))
    (expression_logic - a)
    )
    -111
  )
))

;see expression_logic
(define mult_imp (lambda (a) 
  (expression_logic * a)
))

;see expression_logic
(define div_imp (lambda (a) 
  (expression_logic / a)
))

;see expression_logic
(define mod_imp (lambda (a) 
  (expression_logic modulo a)
))

;helper function for power_imp
(define power_rec (lambda (x y) 
    (if (= y 0)
      1
      (if (= (- y 2) 0)
        (+ (* x x) 0.0)
        (* x (power_rec x (- y 1)))
      ) 
    )
  )
)

;see expression_logic 
(define power_imp (lambda (a) 
  (expression_logic power_rec a)
))

;Name: sqrt_imp
;Purpose: runs sqrt function
;inputs: a - vector with arguments
;outputs: result of sqrt function
;side effects: none
;Recursive: No
(define sqrt_imp (lambda (a) 
  (sqrt  (clean_brakets_spotless (car a)) )
))

;See clean_brakets
(define clean_brakets_front (lambda (line) 
  (if (string=? (substring line 0 1) "(")
    (substring line 1 (string-length line))
    line
  )
))

;See clean_brakets
(define clean_brakets_back (lambda (line) 
  (if (string=? (substring line 
      (- (string-length line) 1) 
      (string-length line)) ")")
    (substring line 0 (- (string-length line) 1) )
    line
  )
))

;Name: clean_brakets
;Purpose: to remove one layer of brackes of a string
;inputs: string with or without brackets
;outputs: string with one layer less of brackets
;side effects: none
;Recursive: No
(define clean_brakets (lambda (line) 
  (clean_brakets_back (clean_brakets_front line))
))

;Name: clean_brakets_spotless
;Purpose: removes all brackets from a string
;inputs: string with or without brackets
;outputs: string without brackets
;side effects: none
;Recursive: No
(define clean_brakets_spotless (lambda (line)
  (if (string=? (substring (clean_brakets line) 0 1) "(")
    (clean_brakets_spotless (clean_brakets line))
    (if (string=? (substring (clean_brakets line) 
        (- (string-length (clean_brakets line)) 1) 
        (string-length (clean_brakets line))) ")")
      (clean_brakets_spotless (clean_brakets line))
      (clean_brakets line)
    )
  )
))

(define *lable-table* (make-hash))
(define *function-table* (make-hash))
(define *variable-table* (make-hash))

(for-each
  (lambda (item) (hash-set! *function-table* (car item) (cadr item)))
    `(
      ("input" ,input_imp)
      ("dim" ,dim_imp)
      ("let" ,let_imp)
      ("if" ,if_imp)
      ("goto" ,goto_imp)
      ("print" ,print_imp)
      ("+" ,add_imp)
      ("-" ,sub_imp)
      ("*" ,mult_imp)
      ("/" ,div_imp)
      ("%" ,mod_imp)
      ("^" ,power_imp)
      ("sqrt" ,sqrt_imp)
    )
)


(for-each
  (lambda (item) (hash-set! *variable-table* (car item) (cadr item)))
    `(
      ("pi" , 3.1415926535)
      ("e" , 2.7182818284)
    )
)


(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))


;Name: statement_parser
;Purpose: Reads in and executes a line from the program 
;inputs: line - a line from the program
;outputs: result of execution 
;side effects: This is called recursively all over this program
;Recursive: Of Course!
(define statement_parser (lambda (line)
  (if (hash-has-key? *lable-table* (car line))
    (statement_parser (cdr line)) ;recursive part
    (if (hash-has-key? *function-table* (clean_brakets (car line)) )
      ((hash-ref *function-table* (clean_brakets (car line))) 
        (cdr line)
      ) 
      (if (hash-has-key? *variable-table* (car line))
        ;base case
        (hash-ref *variable-table* (clean_brakets (car line))) 
        -111
      )
    )
  )
))

;Name: lable_maker
;Purpose: inserts lables to lable table
;inputs: a line from the program
;outputs: none
;side effects: stated above
;Recursive: No
(define lable_maker (lambda (line) (begin
  ;(display (~a line)) (newline)
  (if (<= (length (string-split line)) 1) 
    (display "")
    (if (string?  (substring line 3 4) )
      (if (string=? (substring (cadr (string-split line)) 0 1) "(")
        (display "")
        (hash-set! *lable-table* 
          (clean_brakets (cadr (string-split line)) )
          (clean_brakets (car (string-split line)) ) ;line number
        )
      )
      (display "")
    )
  )
)))

;Name: control_flow
;Purpose: resposible for controling the flow of the program
;inputs: list of lines, current line number
;outputs: none
;side effects: may change which line is being read next
;Recursive: Yes
(define control_flow (lambda (lines ln_num)
  (define le_results (make-vector 1))
  (if (> (length lines) ln_num)
    ;recursive case
    (if (eq? (length  (string-split (~a (list-ref lines ln_num) ) )) 1)
      (control_flow lines (+ ln_num 1))
      (begin
        (vector-set! le_results 0 
            (~a (statement_parser 
              (cdr (string-split (~a (list-ref lines ln_num)))) 
            ))
        )
        (case (car (string-split (vector-ref le_results 0)))
          ( ("goto_stop") 
            (control_flow lines
              (- (string->number 
                (cadr (string-split (vector-ref le_results 0) ))
              ) 1)
            )
          )
          ( ("if_stop") (control_flow lines
              (- (string->number 
                (cadr (string-split (vector-ref le_results 0) ))
              ) 1)
            )
          )
          (else (control_flow lines (+ ln_num 1)) )
        )
      )
    )
    (display "") ;base case
  )
))

;Name: write-program-by-line
;Purpose: top lvl looper of the program. 
;inputs: the program
;outputs: none
;side effects: runs through program 3 times... =[ 
;Recursive: No
(define (write-program-by-line filename program)
    ;Make lables on first pass
    (define lines (map (lambda (line) line) program) )
    (map (lambda (line) (lable_maker (~a line) )) program) 
    ;Start program
    (control_flow lines 0)
)



(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

(main (vector->list (current-command-line-arguments)))


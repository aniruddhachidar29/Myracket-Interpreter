#lang racket
(require racket/struct)
(provide (all-defined-out))
(require "defs.rkt")
(require "examples.rkt")

(define stacks (make-vector 100))
(define stacksindex 0)

;;Global definitions. A counter that tracks the framenumber
(define framenumber 0)

;The stack and its operations. I have decided to make the stack a global.
(define stack '())
(define (push frame) (set! stack (cons frame stack)))
(define (pop) (if (null? stack) (error "Empty stack")
                        (set! stack (cdr stack))))
(define (top) (if (null? stack) (error "Empty stack")
                        (car stack)))


;createframe creates a new frame. It gets its number from
;;framenumber and as a side effect increases framenumber
(define (createframe hashtable parent) ;hashtable gives the initial bindings
   (begin (set! framenumber (+ 1 framenumber)) (frame (- framenumber 1) hashtable parent)))
;This creates the global frame, which, in our case, contains
;empty bindings.
(push (createframe (make-hash '()) (emptyframe)))

;This interprets a program. It uses the following function processdef.
(define (eval-program prog)
         (match prog
           [(pgm deflist) (begin (map (lambda (x) (processdef x (top))) deflist)
                                 (return-value-of-main (top)))]))    ;;;;doubt in car stack term 

;;processdef describes how each definition is processed and added to
;;the frame fr.
(define (processdef defn fr)
  (match defn    
    [(def v/f exp) (begin (hash-set! (frame-bindings fr) v/f (eval-exp exp)))])) ;;;;; me : last stack term may need to be changed (closure exp (last stack))

;; We have said that the result of the program is the value of
;; the symbol main. main must be a defined symbol in each program.
(define (return-value-of-main frame)
  (hash-ref! (frame-bindings frame) 'main "main not found"))

(define (main-frame stack)
  (if (equal? (frame-parent (car stack)) (emptyframe)) (car stack)
      (main-frame (cdr stack))))

;; The expression evaluator is the heart of the interpreter.
;; It will use the functions below
(define (eval-exp exp)
  (cond [(symbol? exp) (if (equal? (search exp (top)) (emptyframe)) (error "Symbol Not Found") (hash-ref (frame-bindings (search exp (top))) exp))]  ; prog4 is giving error
        [(boolean? exp) exp]
        [(number? exp) exp]
        [(list? exp) exp]
       ; [(list? exp) (if (null? exp) exp (begin (displayln (map eval-exp exp)) (last (map eval-exp exp))))]  ;nonononononono
        [(string? exp) exp]
        [else (match exp
                [(uexp op exp1) (op (eval-exp exp1))]
                [(bexp op exp1 exp2) (op (eval-exp exp1) (eval-exp exp2))]
                [(iff cond exp1 exp2) (if (eval-exp cond) (eval-exp exp1) (eval-exp exp2))] 
                [(lam var appli) (closure (lam var appli) (top))] ;;;; 
                [(app exp1 explist) (begin (define closure-eval (eval-exp exp1))
                                                       (push (createframe (make-hash (zip1 (lam-varlist (closure-lambda closure-eval)) (map eval-exp explist)))
                                                                          (closure-frame closure-eval)))
                                                       (define ans (eval-exp (lam-exp (closure-lambda closure-eval))))
                                                       (pop)
                                                       ans)]    ;;;; new frame needed ??
                [(sett var exp) (hash-set! (frame-bindings (search var (top))) var (eval-exp exp))]
               [(lett deflist exp) (process-lets deflist exp)]
                [(defexp deflist exp) (begin (map (lambda (x) (processdef x (top))) deflist) (eval-exp exp))] 
                [(lets deflist exp) (cond ((null? (cdr deflist)) (process-lets deflist exp))
                                          (else (process-lets (list (car deflist))  (lets (cdr deflist) exp))))]
                [(beginexp explist) (process-beginexp explist)]
                ;...and so on, fill in these...
                [(debugexp) (begin
                              (vector-set! stacks stacksindex stack)
                              (set! stacksindex (+ 1 stacksindex))
                              ;(print-current-environment (top))
                              )])]))   

(define (zip1 l1 l2)
  (if (or (null? l1) (null? l2)) '()
  (cons (cons (car l1) (car l2)) (zip1 (cdr l1) (cdr l2)))))

;;An auxilliary function that processes a begin expression
(define (process-beginexp explist)
  (match explist
    [(cons a b) (if (null? b) (eval-exp a)
                    (begin (eval-exp a) (process-beginexp (cdr explist))))]
   ))

;;An auxilliary function that processes a let expression.
;;The let definitions are in deflist, and the let body is exp.
(define (process-lets deflist exp)
  (begin (define fr (createframe (make-hash '()) (top))) (map (lambda (x) (processdef x fr)) deflist)
                                          (push fr)
                                          
                                          (define ans3 (eval-exp exp))
                                          (pop)
                                           ans3))

;;Prints the current environment running through a chain of frames.
;;Note that my struct definitions are such that if fr is a frame,
;;then (displayln fr) will print the frame in the format that I have
;;shown. 
(define (print-current-environment fr)
  (displayln '@@@@@@@@@@@@@@@@@@@@@@@)
  (begin (cond ((not (equal? fr (emptyframe))) (begin (displayln fr) (print-current-environment (frame-parent fr))))))
  ;(displayln '@@@@@@@@@@@@@@@@@@@@@@@))
  )

;;Search for the symbol sym in an environment that starts with the frame
;;fr. We shall make search return either the  emptyframe
;;or the frame containing the symbol (note, not the value of the
;;symbol itself.

(define (search sym fr)
(cond ((equal? fr (emptyframe)) (emptyframe))
      ((hash-ref (frame-bindings fr) sym #f) fr)
      (else (search sym (frame-parent fr)))))
               


;(cond ((null? explist)  (begin (push (createframe (make-hash '()) (top)))    ;;;;;;(closure-frame (eval-exp exp1))change top to else wala term me jo hai ??
;                                                                   ;(push (createframe (make-hash '()) (top)))
;                                                                   (define ans2 (eval-exp (lam-exp (closure-lambda (eval-exp exp1)))))
;                                                                  (pop)
;                                                                  ans2))
;                                          (else (begin (define closure-eval (eval-exp exp1))
;                                                       (push (createframe (make-hash (zip (lam-varlist (closure-lambda closure-eval)) (map eval-exp explist)))
;                                                                          (closure-frame closure-eval)))
;                                                       (define ans (eval-exp (lam-exp (closure-lambda closure-eval))))
;                                                       (pop)
;                                                       ans))
;                                          )


(define (cleanup)
  (set!  stacks (make-vector 100))
  (set! stacksindex 0)
  (set! framenumber 0)
  (set! stack '())
  (push (createframe (make-hash '()) (emptyframe))))
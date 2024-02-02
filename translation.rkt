#lang typed/racket/no-check

;; in this file I translate (most of) the code presented in the
;; given chapter of PLAI from the plai-typed language to the
;; typed/racket/no-check language.

;; in many cases, shriram redefines the same function multiple
;; times; putting all of these definitions in the same file would
;; prevent it from compiling. In these cases, I've commented out
;; the later redefinitions using the s-exp-comment-block #; sequence.

;; in other cases, I've moved test cases downward to allow code to
;; run.

;; also, in order to get them all into one file, I've used DrRacket's
;; explicit `module' feature. You could also take all of these module
;; forms and put each one in its own file. So, for instance, if you
;; take the code in the 'chapter1' module and put in in its own file
;; (with or without the 'module' wrapper, actually) it should run
;; the same way it does here (and without needing the 'require' at
;; the bottom).


;; this module is just a wrapper for the tstruct macro:
(module ts typed/racket/no-check

  ;; this macro defines a "tstruct" form that's just
  ;; like "struct" but automatically inserts the #:transparent
  ;; tag. The examples below use this form instead of struct.
  ;; If you want to cut and paste the code below into your own
  ;; code, you can either
  ;; a) copy and paste this macro into your code, or
  ;; b) change "tstruct" into "struct" and add the "#:transparent"
  ;;    tag to each one.
  (define-syntax tstruct
    (syntax-rules ()
      [(_ name fields)
       (struct name fields #:transparent)]))

  (provide tstruct))


;; CHAPTER 1:
(module chapter1 typed/racket/no-check
  (require typed/rackunit
           ;; see the previous comment on the ts module; this
           ;; is how we import the 'tstruct' binding.
           (submod ".." ts))
  
  (define-type MisspelledAnimal (U caml yacc))
  (tstruct caml ([humps : Real]))
  (tstruct yacc ([height : Real]))

  (caml 2)
  (yacc 1.9)

  (define ma1 (caml 2))
  (define ma2 (yacc 1.9))

  (define (good? [ma : MisspelledAnimal]) : Boolean
    (match ma
      [(caml humps) (>= humps 2)]
      [(yacc height) (> height 2.1)]))

  ;; redefinition 1:
  #;(define (good? [ma : MisspelledAnimal]) : Boolean
      (match ma
        [(caml h) (>= h 2)]
        [(yacc h) (> h 2.1)]))

  ;; redefinition 2:
  #;(define (good? [ma : MisspelledAnimal]) : Boolean
      (cond
        [(caml? ma) (>= (caml-humps ma) 2)]
        [(yacc? ma) (> (yacc-height ma) 2.1)]))

  (check-equal? (good? ma1) #t)
  (check-equal? (good? ma2) #f)
  )
;; CHAPTER 2:

(module chapter2 typed/racket/no-check
  (require typed/rackunit
           (submod ".." ts))
  
  (define-type ArithC (U NumC PlusC MultC))
  (tstruct NumC ([n : Real]))
  (tstruct PlusC ([l : ArithC] [r : ArithC]))
  (tstruct MultC ([l : ArithC] [r : ArithC]))

  #;(define (parse [s : Sexp]) : ArithC
    (cond
      [(real? s) (numC s)]
      [(list? s)
       (case (first s)
         [(+) (plusC (parse (second s)) (parse (third s)))]
         [(*) (multC (parse (second s)) (parse (third s)))]
         [else (error 'parse "invalid list input")])]
      [else (error 'parse "invalid input")]))

  ;; but actually here's a version that uses match instead of case...
  (define (parse-using-match [s : Sexp]) : ArithC
    (match s
      [(? real? n) (NumC n)]
      [(list '+ l r) (PlusC (parse-using-match (second s)) (parse-using-match (third s)))]
      [(list '* l r) (MultC (parse-using-match (second s)) (parse-using-match (third s)))]
      [other (error 'parse "invalid input: ~e" s)]))

  (check-equal? (parse-using-match '(+ (* 1 2) (+ 2 3)))
                (PlusC (MultC (NumC 1) (NumC 2))
                       (PlusC (NumC 2) (NumC 3)))))

;; CHAPTER 3:

(module chapter3 typed/racket/no-check

  (require (submod ".." ts))

  (define-type ArithC (U numC plusC multC))
  (tstruct numC ([n : Real]))
  (tstruct plusC ([l : ArithC] [r : ArithC]))
  (tstruct multC ([l : ArithC] [r : ArithC]))

  #;(define (interp [a : ArithC]) : Real
      (match a
        [(numC n) n]
        [(plusC l r) ...]
        [(multC  l r) ...]))

  ;; in Typed Racket, this produces type errors...:
  #;(define (interp [a : ArithC]) : Real
      (match a
        [(numC n) n]
        [(plusC l r) (+ l r)]
        [(multC  l r) (+ l r)]))

  (define (interp [a : ArithC]) : Real
    (match a
      [(numC n) n]
      [(plusC l r) (+ (interp l) (interp r))]
      [(multC  l r) (* (interp l) (interp r))]))

  (display 13))


(module chapter4 typed/racket/no-check

  (require (submod ".." ts))

  (define-type ArithC (U NumC PlusC MultC))
  (tstruct NumC ([n : Real]))
  (tstruct PlusC ([l : ArithC] [r : ArithC]))
  (tstruct MultC ([l : ArithC] [r : ArithC]))
 
  (define-type ArithS (U NumS PlusS BMinusS MultS))
  (struct NumS ([n : Real]))
  (struct PlusS ([l : ArithS] [r : ArithS]))
  (struct BMinusS ([l : ArithS] [r : ArithS]))
  (struct MultS ([l : ArithS] [r : ArithS]))

  ;; <desugar> ::=
  (define (desugar [as : ArithS]) : ArithC
    (match as
      [(NumS n) (NumC n)]
      [(PlusS l r) (PlusC (desugar l) (desugar r))]
      [(MultS l r) (MultC (desugar l) (desugar r))]
      #;(<BMinus-case>)))

  ;; <bminusS-case> :=
  #;[(BMinusS l r) (PlusC (desugar l) (MultC (NumC -1)
                                           (desugar r)))]
  )

(module chapter5 typed/racket/no-check

  ;; SECTION 5.1
  
  (require (submod ".." ts))
  (tstruct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]))

  ;; <exprC> ::=
  (define-type ExprC (U NumC IdC AppC PlusC MultC))
  (tstruct NumC ([n : Real]))
  ;; <idC-def>
  ;; <app-def>
  (tstruct PlusC ([l : ExprC] [r : ExprC]))
  (tstruct MultC ([l : ExprC] [r : ExprC]))

  ;;<idC-def> ::=
  (tstruct IdC ([s : Symbol]))
  ;;<app-def> ::=
  (tstruct AppC ([fun : Symbol] [arg : ExprC]))

  (FunDefC 'double 'x (PlusC (IdC 'x) (IdC 'x)))
  (FunDefC 'quadruple 'x (AppC 'double (AppC 'double (IdC 'x))))
  (FunDefC 'const5 '_ (NumC 5))

  ;; SECTION 5.2    

  ;;<interp> ::=
  (define (interp [e : ExprC] [fds : (Listof FunDefC)]) : Real
    ;;<interp-body>
    #xfeedface
    )

  ;;<interp-body> ::=

  #;(match e
      [(NumC n) n]
      ;<idC-interp-case>
      ;<appC-interp-case>    
      [(PlusC l r) (+ (interp l fds) (interp r fds))]
      [(MultC l r) (* (interp l fds) (interp r fds))])

  ;; SECTION 5.3
  
  ;;<subst> ::=
  (define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
    ;;<subst-body>
    (NumC #xfeedface)
    )

  
  ;;<subst-body> ::=
  #;(match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(AppC f a) (AppC f (subst what for a))]
    [(PlusC l r) (PlusC (subst what for l)
                        (subst what for r))]
    [(MultC l r) (MultC (subst what for l)
                        (subst what for r))])

  ;; SECTION 5.4

  ;;<appC-interp-case-take-1> ::=

  #;[(AppC f a) (define fd (get-fundef f fds))
                (subst a
                       (FunDefC-arg fd)
                       (FunDefC-body fd))]

  ;; <appC-interp-case> ::=

  #;[(AppC f a) (define fd (get-fundef f fds))
                (interp (subst a
                               (FunDefC-arg fd)
                               (FunDefC-body fd))
                        fds)]

  ;;<idC-interp-case> ::=
  
  #;[(IdC _) (error 'interp "shouldn't get here")]
      



  (define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
    (cond
      [(empty? fds)
       (error 'get-fundef "reference to undefined function")]
      [(cons? fds)
       (cond
         [(equal? n (FunDefC-name (first fds))) (first fds)]
         [else (get-fundef n (rest fds))])]))


  
  )

(module chapter6 typed/racket/no-check

  ;; copied from chapter 5:
  (require typed/rackunit
           (submod ".." ts))
  
  (tstruct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]))

  ;; <exprC> ::=
  (define-type ExprC (U NumC IdC AppC PlusC MultC))
  (tstruct NumC ([n : Real]))
  ;; <idC-def>
  ;; <app-def>
  (tstruct PlusC ([l : ExprC] [r : ExprC]))
  (tstruct MultC ([l : ExprC] [r : ExprC]))

  ;;<idC-def> ::=
  (tstruct IdC ([s : Symbol]))
  ;;<app-def> ::=
  (tstruct AppC ([fun : Symbol] [arg : ExprC]))


  ;; chapter 6 starts here: 
  (tstruct Binding ((name : Symbol) (val : Real)))
 
  (define-type Env (Listof Binding))
  (define mt-env '())
  (define extend-env cons)


  (define (interp [expr : ExprC] [env : Env] [fds : (Listof FunDefC)]) : Real
    (match expr
      [(NumC n) n]
      ;<idC-case>
      ;<appC-case>
      ;<plusC/multC-case>
      ))

  ; <plusC/multC-case> ::=

  #;[(PlusC l r) (+ (interp l env fds) (interp r env fds))]
  #;[(MultC l r) (* (interp l env fds) (interp r env fds))]

  ; <idC-case> ::=

  #;[(IdC n) (lookup n env)]

  ; <appC-case> ::=

  #;[(AppC f a)
     (define fd (get-fundef f fds))
     <appC-interp>]

  ; <appC-interp> ::=

  #;(interp (FunDefC-body fd)
            <appC-interp-bind-in-env>
            fds)

  ; <appC-interp-bind-in-env-take-1> ::=

  #;(extend-env (bind (FunDefC-arg fd)
                      (interp a env fds))
                env)

  (define (lookup [for : symbol] [env : Env]) : number
    (match env
      ['() (error 'lookup "name not found: ~e" for)]
      [(cons (Binding name val) r) (cond
                    [(symbol=? for name) val]
                    [else (lookup for r)])]))

  #;(check-equal? (interp (PlusC (NumC 10) (AppC 'const5 (NumC 10)))
                        mt-env
                        (list (FunDefC 'const5 '_ (NumC 5))))
                15)
 
  #;(check-equal? (interp (PlusC (NumC 10) (AppC 'double (PlusC (NumC 1) (NumC 2))))
                        mt-env
                        (list (FunDefC 'double 'x (PlusC (IdC 'x) (IdC 'x)))))
                16)
 
  #;(check-equal? (interp (PlusC (NumC 10) (AppC 'quadruple (PlusC (NumC 1) (NumC 2))))
                        mt-env
                        (list (FunDefC 'quadruple 'x (AppC 'double (AppC 'double (IdC 'x))))
                              (FunDefC 'double 'x (PlusC (IdC 'x) (IdC 'x)))))
                22)

  #;(interp (AppC 'f1 (NumC 3))
          mt-env
          (list (FunDefC 'f1 'x (AppC 'f2 (NumC 4)))
                (FunDefC 'f2 'y (PlusC (IdC 'x) (IdC 'y)))))



  ; <appC-interp-bind-in-env> ::=

  #;(extend-env (Binding (FunDefC-arg fd)
                         (interp a env fds))
                mt-env)
)




;; to run a particular chapter's code:
(require 'chapter1)
(require 'chapter2)
(require 'chapter3)
(require 'chapter4)
(require 'chapter5)
(require 'chapter6)

#lang typed/racket

(require typed/rackunit)
;(require/typed racket/mutability [immutable-hash? (Any -> Boolean)])

; ExprC Definition, from SECTION 5.1 Book
(define-type ExprC (U NumC IdC AppC binopC ifleq0?))
(struct NumC ([n : Real]) #:transparent)                    ; Number 
(struct IdC ([s : Symbol]) #:transparent)                   ; Id 
(struct AppC ([fun : Symbol] [arg : ExprC]) #:transparent)  ; Function Application
(struct ifleq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent ) ; 0 >= x 

(struct binopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ; Takes care of binary operations 

; parse: takes in s expression, returns ExprC
; 4 EBNF Definition from assignment 
(define (parse [a : Sexp]) : ExprC
  (match a
    [(? real? n) (NumC n)] ; num 
    [(list '+ a b ) (binopC '+ (parse a) (parse b))] ; { + ExprC ExprC } 
    [(list '- a b ) (binopC '- (parse a) (parse b))] ; { - ExprC ExprC } 
    [(list '* a b ) (binopC '* (parse a) (parse b))] ; { * ExprC ExprC } 
    [(list '/ a b ) (binopC '/ (parse a) (parse b))] ; { / ExprC ExprC }
    [(list (? symbol? n) exp) (AppC n (parse exp))]  ; AppC {id ExprC} function call 
    [(? symbol? n) (IdC n)]                      ; id
    ;[(list (? Idc? a) b) (IdC a)]               ; idC (id expr)  ??? idk how to do  is it needed? 
    [(list 'ifleq0? test then else) (ifleq0? (parse test) (parse then) (parse else))] ; {ifleq0? ExprC ExprC ExprC}
    [other (error 'parser "OAZO failed: ~a is invalid" a)]))


(check-equal? (parse '{- {+ 4 2 } 3}) (binopC '- (binopC '+ (NumC 4) (NumC 2)) (NumC 3)))
(check-equal? (parse '{/ {* 4 2 } 3}) (binopC '/ (binopC '* (NumC 4) (NumC 2)) (NumC 3)))
(check-equal? (parse 'fasd) (IdC 'fasd))
(check-equal? (parse '{ifleq0? 5 4 2}) (ifleq0? (NumC 5) (NumC 4) (NumC 2)))
(check-exn (regexp
     (regexp-quote "parser: OAZO failed: (Amongus In Real Life (SUSSYBAKA)) is invalid"))
     (lambda () (parse '{Amongus In Real Life {SUSSYBAKA}})))


; Function definition (GIVEN)
(struct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent) ; function definition

; parse-fundef: Parses a function definition
; s: S expression for the function definition
; returns a FunDefC struct of the function 
(define (parse-fundef [s : Sexp] ) : FunDefC
  (match s
    [(list 'func (list (? symbol? name) (? symbol? arg)) ': (list body ...))
     (FunDefC name arg (parse body))]
     ;[(list 'func (list 'main 'init) ': (list body ...))     ; should parse-fundef parse main in here? 
     ;(FunDefC 'main 'init (parse body))] ; main function 
    [other (error 'parse-fundef "OAZO failed: ~a is invalid" s)]))

(define double (FunDefC 'double 'x (binopC '+ (IdC 'x) (IdC 'x))))

(check-equal? (parse-fundef '{func {double x} : {+ x x}}) double)
(check-equal? (parse-fundef '{func {f x} : {+ x 14}}) (FunDefC 'f 'x (binopC '+ (IdC 'x) (NumC 14))))
(check-exn (regexp
     (regexp-quote "parse-fundef: OAZO failed: (ISA President) is invalid"))
     (lambda () (parse-fundef '{ISA President})))

;(parse-prog '{{func {f x} : {+ x 14}}
;              {func {main init} : {f 2}}})

; parse-prog: Parses a program TODO
;(define (parse-prog [s : Sexp]) : (Listof FundefC)
;  (1))

; interp-fns
; Interprets the function named main from the function definitions.
;(define (interp-fns [funs : (Listof FunDefC)] ) : Real (1))


; get-fundef: helper function to map/look up the name to the function definition
; n: the function name
; fds: a list of function definitions
; returns the function definition 
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds)
     (error 'get-fundef "OAZO reference to undefined function, ~a" n)]
    [(cons? fds)
     (cond
       [(equal? n (FunDefC-name (first fds))) (first fds)]
       [else (get-fundef n (rest fds))])]))


; subst: Replaces the name with another expression
; what: what we want to replace the name with
; for: what name we want to perform substitution
; in: expression we want to do it in
; returns the expression that is substituted with 
(define (subst [what : ExprC] [for : Symbol ] [in : ExprC] ) : ExprC
  (match in
    [(NumC n ) in ]
    [(IdC s) (cond 
               [(symbol=? s for) what]
               [else in])]
    [(AppC fun arg) (AppC fun (subst what for arg))]
    [(binopC sym l r) (binopC sym (subst what for l ) (subst what for r ))]
    [other (error 'subst "OAZO failed: ~a is invalid" in)]))


; hash table for operator names and meanings
(define binopHash (hash '+ + '- - '* * '/ /))
;(immutable-hash? binopHash)

; interp: Interprets the given expression, using the list of funs to resolve applications
; exp: expression given
; fds: list of defined functions
; returns a real number after computed 
(define (interp [exp : ExprC] [fds : (Listof FunDefC)]) : Real
  (match exp
    [(NumC n) n]
    [(IdC  n) (error 'interp "OAZO shouldn't get here, ~a" exp)]
    [(AppC f a) (define fd (get-fundef f fds)) (interp (subst a
                                                              (FunDefC-arg fd)
                                                              (FunDefC-body fd))
                                                       fds)] ; idk 
    [(binopC s l r) (cond
                      [(hash-has-key? binopHash s)
                       ((hash-ref binopHash s) (interp l fds) (interp r fds))]
                      [else (error 'interp "OAZO failed: ~a is invalid" exp)])]
    ;[(binopC '+ l r) (+ (interp l fds) (interp r fds))]
    ;[(binopC '- l r) (- (interp l fds) (interp r fds))]
    ;[(binopC '* l r) (* (interp l fds) (interp r fds))]
    ;[(binopC '/ l r) (/ (interp l fds) (interp r fds))]
    [(ifleq0? test then else) (cond
                                [(>= 0 (interp test fds)) (interp then fds)]
                                [else (interp else fds)])]
    [other (error 'interp "OAZO failed: ~a is invalid" exp)]))

;(interp (binopC '% (NumC 3) (NumC 3)) '()) ; test binopHash error
; (define double (FunDefC 'double 'x (binopC '+ (IdC 'x) (IdC 'x))))
; (define addOne (FunDefC 'addOne 'x (binopC '+ (IdC 'x) (NumC 1))))
(define square (parse-fundef '{func {square x} : {* x x}}))
(define add-one (parse-fundef '{func {add-one x} : {+ x 1}})) ; help me idk whats going on 
(define funcList (list double add-one square)) ; function list 

; parse -> interpreter 
(printf "~v\n" add-one)
(parse '{add-one 5})
(interp (parse '{add-one 5}) funcList )
(check-equal? (interp (parse '{square {double 2} }) funcList) 16)


; top-interp: combines parsing and evaluation
; accepts an s-expression and calls the parser and then the interp function. (GIVEN)
;(define (top-interp [s : Sexp]) : Real


; find-main: Finds and return the main function
(define (find-main [funs : (Listof FunDefC)]) : ExprC
  (match funs
    ['() (error 'find-main "OAZO failed: no main function found")]
    [(cons (struct FunDefC (name arg body)) r)
     (match name
       ['main
        (match arg
          ['init body]
          [else (error 'find-main "OAZO failed: ~a is an incorrect argument for main" arg)])]
       [else (find-main r)])]))


; interp-fns: Interprets the function named main
(define (interp-fns [funs : (Listof FunDefC)]) : Real
  (define main (find-main funs))
  (match main
    [ExprC (interp main funs)]))
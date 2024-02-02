#lang typed/racket

(require typed/rackunit)

; ExprC Definition, from SECTION 5.1 Book
(define-type ExprC (U NumC IdC AppC binopC ifleq0?))
(struct NumC ([n : Real]) #:transparent)                    ; Number 
(struct IdC ([s : Symbol]) #:transparent)                   ; Id 
(struct AppC ([fun : Symbol] [arg : ExprC]) #:transparent)  ; Function Application
;(struct PlusC ([l : ExprC] [r : ExprC]) #:transparent)     ; +
;(struct SubC ([l : ExprC] [r : ExprC]) #:transparent)      ; -
;(struct MultC ([l : ExprC] [r : ExprC]) #:transparent)     ; *
;(struct DivC ([l : ExprC] [r : ExprC])#:transparent )      ; / 
(struct ifleq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent ) ; 0 >= x 

(struct binopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ; FROM 3.1 Assignment

; parse: takes in s expression, returns ExprC
; 4 EBNF Definition from assignment 
(define (parse [a : Sexp]) : ExprC
  (match a
    [(? real? n) (NumC n)] ; num 
    [(list '+ a b ) (binopC '+ (parse a) (parse b))] ; { + ExprC ExprC } 
    [(list '- a b ) (binopC '- (parse a) (parse b))] ; { - ExprC ExprC } 
    [(list '* a b ) (binopC '* (parse a) (parse b))] ; { * ExprC ExprC } 
    [(list '/ a b ) (binopC '/ (parse a) (parse b))] ; { / ExprC ExprC } 
    [(? symbol? n) (IdC n)]                      ; id
    ;[(list (? Idc? a) b) (IdC a)]               ; idC (id expr)  ??? idk how to do  
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
(define (parse-fundef [s : Sexp] ) : FunDefC
  (match s
    ; '{func {f x} : {+ x 14}}
    [(list 'func (list (? symbol? name) (? symbol? arg)) ': (list body ...))
     (FunDefC name arg (parse body))]
     ;[(list 'func (list 'main 'init) ': (list body ...))
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


; interp
; Interprets the given expression, using the list of funs to resolve applications.
(define (interp [exp : ExprC] [fds : (Listof FunDefC)]) : Real
  (match exp
    [(NumC n) n]
    [(IdC  n) (error 'interp "~a shouldn't get here" exp)]
    [(AppC f a) (define fd (get-fundef f fds)) (interp (subst a
                                                                  (FunDefC-arg fd)
                                                                  (FunDefC-body fd))
                                                           fds)] ; idk 
    [(binopC '+ l r) (+ (interp l fds) (interp r fds))]
    [(binopC '- l r) (- (interp l fds) (interp r fds))]
    [(binopC '* l r) (* (interp l fds) (interp r fds))]
    [(binopC '/ l r) (/ (interp l fds) (interp r fds))]
    ;[(ifleq0? test then else) ]
    [other (error 'interp "OAZO failed: ~a is invalid" exp)]))


; get-fundef: helper function to look up the function definition
  (define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
    (cond
      [(empty? fds)
       (error 'get-fundef "reference to undefined function")]
      [(cons? fds)
       (cond
         [(equal? n (FunDefC-name (first fds))) (first fds)]
         [else (get-fundef n (rest fds))])]))

; subst: replaces the name with another expression 
(define (subst [what : ExprC] [for : Symbol ] [in : ExprC] ) : ExprC
  (match in
    [(NumC n ) in ]
    [(IdC s) (cond 
               [(symbol=? s for) what]
               [else in])]
    [(AppC fun arg) (AppC fun (subst what for arg))]
    [(binopC sym l r) (binopC sym (subst what for l ) (subst what for l ))]
    [other (error 'subst "OAZO failed: ~a is invalid" in)]))
    
; top-interp: combines parsing and evaluation
; accepts an s-expression and calls the parser and then the interp function. (GIVEN)
;(define (top-interp [s : Sexp]) : Real
;  (interp-fns (parse-prog s)))
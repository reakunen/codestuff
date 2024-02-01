#lang typed/racket

(require typed/rackunit)

; ExprC Definition, from SECTION 5.1 Book
(define-type ExprC (U NumC IdC AppC binopC ifleq0?))
(struct NumC ([n : Real]) #:transparent)                   ; Number 
(struct IdC ([s : Symbol]) #:transparent)                  ; Id 
(struct AppC ([fun : Symbol] [arg : ExprC]) #:transparent) ; Function Application
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
(check-exn (regexp (regexp-quote "parser: OAZO failed: (Amongus In Real Life (SUSSYBAKA)) is invalid"))
           (lambda () (parse '{Amongus In Real Life {SUSSYBAKA}})))


; interp: Interpreter
;(define )

; top-interp: combines parsing and evaluation
; accepts an s-expression and calls the parser and then the interp function. (GIVEN)
;(define (top-interp [s : Sexp]) : Real
;  (interp-fns (parse-prog s)))

;; TO-DO

; Function definition (GIVEN)
(struct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent) ; function definition

; parse-fundef: Parses a function definition
(define (parse-fundef [s : Sexp] ) : FunDefC
  (match s
    ; '{func {f x} : {+ x 14}}
    ;(list 'func (list (? symbol? name) (? symbol? arg)) ': (list (? symbol? body) ...) ) (FunDefC name arg (parse body))
    [(list 'func (list (? symbol? name) (? symbol? arg)) ': (list body ...) ) (FunDefC name arg (parse body))]
    [other (error 'parser "OAZO failed: ~a is invalid" s)]))

(parse-fundef '{func {f x} : {+ x 14}})
; parse-prog: Parses a program 
;(define (parse-prog [s : Sexp]) : (Listof FundefC)
;  ())


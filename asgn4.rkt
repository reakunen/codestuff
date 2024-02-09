#lang typed/racket
(require typed/rackunit)

; Finished Asgn3, all test cases passed.

; ExprC Definition, from SECTION 5.1 Book
(define-type ExprC (U NumC IdC AppC binopC ifleq0?))
(struct NumC ([n : Real]) #:transparent)                    ; Number 
(struct IdC ([s : Symbol]) #:transparent)                   ; Id 
(struct AppC ([fun : Symbol] [arg : (Listof ExprC)]) #:transparent)  ; Function Application
(struct ifleq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent ) ; 0 >= x
(struct binopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ; Takes care of binary operations 


; hash table for operator names and meanings
(define binopHash (hash '+ + '- - '* * '/ /))



; parse: takes in an s-expression, returns the associated ExprC  
; 4 EBNF Definition from assignment
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)] ; num 
    [(list '+ a b ) (binopC '+ (parse a) (parse b))] ; { + ExprC ExprC } 
    [(list '- a b ) (binopC '- (parse a) (parse b))] ; { - ExprC ExprC } 
    [(list '* a b ) (binopC '* (parse a) (parse b))] ; { * ExprC ExprC } 
    [(list '/ a b ) (binopC '/ (parse a) (parse b))] ; { / ExprC ExprC }
    [(list (? symbol? n) args ...) (cond
                                [(hash-has-key? binopHash n) (error 'parse "OAZO failed: ~a is invalid IdC" s)]
                                [else (AppC n (map parse args))])]  ; AppC {id ExprC ...} function call 
    [(? symbol? n) (match n  
                     [(or '+ '- '/ '* 'ifleq0? 'func)
                      (error 'parse "OAZO failed: ~a is invalid IdC" s)]
                     [other (IdC n)])]                 ; id
    [(list 'ifleq0? test then else) (ifleq0? (parse test) (parse then) (parse else))]
    [other (error 'parse "OAZO failed: ~a is invalid" s)]))

(parse '{functioncall x 7 3082 cb}) ; can parse functions with multiple args


; Function definition (GIVEN)
(struct FunDefC ([name : Symbol] [arg : (Listof Symbol)] [body : ExprC]) #:transparent)



; parse-fundef: Parses a function definition
; s: S expression for the function definition
; returns a FunDefC struct of the function

(define (parse-fundef [s : Sexp] ) : FunDefC
  (match s
    [(list 'func (list (? symbol? name) (? symbol? args) ...) ': body)
     (match name
       [(or '+ '- '/ '*)
        (error 'parse-fundef "OAZO failed: Function name cannot be ~a" name)]
       [else
        (match args
          [(or '+ '- '/ '*)
           (error 'parse-fundef "OAZO failed: Function argument cannot be ~a" args)]
          [other (if (check-duplicates args)
               (error 'parse-fundef "OAZO failed: Duplicate function arguments ~a" args)
               (FunDefC name (cast args (Listof Symbol)) (parse body)))])])]
[other (error 'parse-fundef "OAZO failed: ~a is invalid" s)]))

(parse-fundef '{func {f} : {+ 2 2}}) ; works with no args and multiple args now 
(parse-fundef '{func {f arg1 arg2 arg3} : {+ 2 2}})
(check-exn (regexp
            (regexp-quote "parse-fundef: OAZO failed: Duplicate function arguments (a a a)"))
           (lambda () (parse-fundef '{func {f a a a} : {+ 2 2}}))) 

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

; parse-prog: Parses a program
; s: the s-expression containing a program
; returns the parsed functions defined in the program
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]
    [other (error 'parse-prog "OAZO failed: ~a is an invalid program" s)]))

; find-main: Finds and return the main function
; funs: list of defined functions
; returns the body of main
(define (find-main [funs : (Listof FunDefC)]) : ExprC
  (match funs
    ['() (error 'find-main "OAZO failed: no main function found")]
    [(cons (struct FunDefC (name arg body)) r)
     (match name
       ['main
        (match arg
          ['() body]
          [else (error 'find-main "OAZO failed: ~a main should have no arguments" arg)])]
       [else (find-main r)])]))


; subst: Replaces the name with another expression
; what: what we want to replace the name with
; for: what name we want to perform substitution
; in: expression we want to do it in
; returns the expression that is substituted with
; (AppC 'functioncall (list (IdC 'x) (NumC 7) (NumC 3082) (IdC 'cb)))
; (parse '{functioncall x 7 3082 cb}) ; can parse functions with multiple args
; (FunDefC 'f '() (binopC '+ (NumC 2) (NumC 2)))
; cons

; name of function and the arguments
;(define interpFunc [] ) : (Listof ExprC)

; takes a list of arguments, that intepred
(define (substHelper [for : (Listof Symbol)] [args : (Listof ExprC)] [body : ExprC]) : ExprC
  (match (list for args)
    [(list '() '()) body]
    [(list '() e) (error 'interp "Not enough arguments" )]
    [(list e '()) (error 'interp "Too many arguments" )]
    [(list (cons f1 r1) (cons f2 r2)) (substHelper r1 r2 (subst f2 f1 body))]))

  ;run subst multiple times 
; what are we substituting?
; go through the list of symbols and match them 
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (if (symbol=? s for) what in)]
    [(AppC fun args) (AppC fun (map (lambda ([arg : ExprC]) (subst what for arg)) args))] ; AppC 'idc ExprC ...
    [(binopC sym l r) (binopC sym (subst what for l) (subst what for r))]
    [(ifleq0? test then else)
     (ifleq0? (subst what for test) (subst what for then) (subst what for else))]))


; interp: Interprets the given expression, using the list of funs to resolve applications
; exp: expression given
; fds: list of defined functions
; returns a real number after computed
(define (interp [exp : ExprC] [fds : (Listof FunDefC)]) : Real
  (match exp
    [(NumC n) n]
    [(IdC n) (error 'interp "OAZO shouldn't get here, ~a" exp)]
    [(AppC f args) 
     (define fd (get-fundef f fds))
     ;(define (substHelper [for : (Listof Symbol)] [args : (Listof ExprC)] [body : ExprC]) : ExprC
     (interp (substHelper 
              (FunDefC-arg fd) ; list of symbols 
              (map (lambda ([arg : ExprC]) (NumC ( interp arg fds)) args)) ; expressions
             (FunDefC-body fd)) 
               fds)]
         ;[(AppC fun args) (AppC fun (map (lambda ([arg : ExprC]) (subst what for arg)) args))
  ;       (interp (subst (NumC ( interp a fds)) ; map 
  ;                      (FunDefC-arg fd)
  ;                      (FunDefC-body fd))
  ;               fds)] ; lambda here 
    [(binopC s l r) (cond
                      [(hash-has-key? binopHash s)
                       (match s
                         ['/ (if (eq? (interp r fds) 0)
                                 (error 'interp "OAZO failed: can't divide by 0")
                                 ((hash-ref binopHash s) (interp l fds) (interp r fds)))]
                         [else ((hash-ref binopHash s) (interp l fds) (interp r fds))])]
                      [else (error 'interp "OAZO failed: ~a is invalid" exp)])]               
    [(ifleq0? test then else) (cond
                                [(>= 0 (interp test fds)) (interp then fds)]
                                [else (interp else fds)])]))


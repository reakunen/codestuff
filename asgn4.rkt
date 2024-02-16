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
    [(list 'ifleq0? test then else) (ifleq0? (parse test) (parse then) (parse else))]
    [(? symbol? n) (match n  
                     [(or '+ '- '/ '* 'ifleq0? 'func ':)
                      (error 'parse "OAZO failed: ~a is invalid IdC" s)]
                     [other (IdC n)])]                 ; id
    [(list (? symbol? n) args ...) (match n
                                [(or '+ '- '/ '* 'ifleq0? 'func ':) (error 'parse "OAZO failed: ~a is invalid IdC" s)]
                                [other (AppC n (map parse args))])]  ; AppC {id ExprC ...} function call 
    [other (error 'parse "OAZO failed: ~a is invalid" s)]))
    

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
        (match args ; map args, args: array 
;          [(or '+ '- '/ '*)
;           (error 'parse-fundef "OAZO failed: Function argument cannot be ~a" args)]
          [other (if (check-duplicates args)
               (error 'parse-fundef "OAZO failed: Duplicate function arguments ~a" args)
               (FunDefC name (cast args (Listof Symbol)) (parse body)))])])]
[other (error 'parse-fundef "OAZO failed: ~a is invalid" s)]))



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



; interprets a list of arguments 
; for: from FunDefC, the argument names
; args: from your function call (AppC (ExprC's)
; body: the function body that you have to replace 
(define (substHelper [for : (Listof Symbol)] [args : (Listof ExprC)] [body : ExprC]) : ExprC
  (match (list for args)
    [(list '() '()) body]
    [(list  e '()) (error 'substHelper "OAZO failed: Not enough arguments" )]
    [(list '() e) (error 'substHelper "OAZO failed: Too many arguments" )]
    [(list (cons f1 r1) (cons f2 r2)) (substHelper r1 r2 (subst f2 f1 body))]))



; subst: Replaces the name with another expression
; what: what we want to replace the name with
; for: what name we want to perform substitution
; in: expression we want to do it in
; returns the expression that is substituted with
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (if (symbol=? s for) what in)]
    [(AppC fun args) (AppC fun (map (lambda ([arg : ExprC]) (subst what for arg)) args))] ; AppC 'idc ExprC ...
    [(binopC sym l r) (binopC sym (subst what for l) (subst what for r))]
    [(ifleq0? test then else)
     (ifleq0? (subst what for test) (subst what for then) (subst what for else))]))
what if we did if wazo4 implementation using environment instead of subst, what would the difference be concrete syntax to value output

filter square > 50 
1) convert list into cons 
2) writing python in terms of 
3) wazo implenetation of subst that is wrong that if then else clause wirte a test case that catches error 
write a test case for subst: it does not substitute the if then else clause 
'(a (b c))
; interp: Interprets the given expression, using the list of funs to resolve applications
; exp: expression given
; fds: list of defined functions
; returns a real number after computed
(define (interp [exp : ExprC] [fds : (Listof FunDefC)]) : Real
  (match exp
    [(NumC n) n]
    [(IdC n) (error 'interp "OAZO shouldn't get here, ~a" exp)]
    ;(struct AppC ([fun : Symbol] [arg : (Listof ExprC)]) #:transparent)  ; Function Application
    [(AppC f args) 
     (define fd (get-fundef f fds))
     ;(define (substHelper [for : (Listof Symbol)] [args : (Listof ExprC)] [body : ExprC]) : ExprC
     (interp (substHelper 
              (FunDefC-arg fd) ; list of symbols 
              (map (lambda ([arg : ExprC]) (NumC (interp arg fds))) args) ; expressions
             (FunDefC-body fd)) 
               fds)]
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



; interp-fns: Interprets the function named main
; funs: list of defined functions
; returns the evaluated real number of the program
(define (interp-fns [funs : (Listof FunDefC)]) : Real
  (define main (find-main funs))
  (match main
    [ExprC (interp main funs)]))



; top-interp: combines parsing and evaluation
; accepts an s-expression and calls the parse function and then the interp function. (GIVEN)
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))



;-------------------TEST CASES----------------------

; Definitions for test cases
(define double (FunDefC 'double '(x) (binopC '+ (IdC 'x) (IdC 'x))))

(define square (parse-fundef '{func {square x} : {* x x}}))
(define add-one (parse-fundef '{func {add-one x} : {+ x 1}}))
(define sub-ten (parse-fundef '{func {sub-ten x} : {- x 10}}))
(define div-zero (parse-fundef '{func {div-zero x} : {/ x 0}}))
(define funcList (list double add-one square sub-ten div-zero)) ; function list 

(define fds (list (FunDefC 'add-one '(x) (binopC '+ (IdC 'x) (NumC 1)))))


; top-interp tests
(check-equal? (top-interp '{{func {f x} : {+ x 14}}
                            {func {main} : {f 2}}}) 16)

(check-equal? (top-interp '{{func {double x} : {+ x x}}
                            {func {main} : {double {double 2}}}}) 8)

(check-exn (regexp
            (regexp-quote "get-fundef: OAZO reference to undefined function, amongus"))
           (lambda () (top-interp '{{func {sussybaka x} : {+ x x}}
                                    {func {main} : {amongus {amongus 2}}}})))

(check-exn (regexp
            (regexp-quote "find-main: OAZO failed: (sus) main should have no arguments"))
           (lambda () (top-interp '{{func {sussybaka x} : {+ x x}}
                                    {func {main sus} : {amongus {amongus 2}}}})))

(check-exn (regexp
            (regexp-quote "find-main: OAZO failed: no main function found"))
           (lambda () (top-interp '{{func {sussybaka x} : {+ x x}}
                                    {func {baka sus} : {amongus {amongus 2}}}})))

(check-exn (regexp
            (regexp-quote "substHelper: OAZO failed: Not enough arguments"))
           (lambda () (top-interp '{{func {f x y} : {+ x y}}
                            {func {main} : {f 2}}})))

(check-exn (regexp
            (regexp-quote "substHelper: OAZO failed: Too many arguments"))
           (lambda () (top-interp '{{func {f x y} : {+ x y}}
                            {func {main} : {f 69 2 3 2}}})))

(check-equal? (top-interp
               '((func (main) :
                       (+ (f 13) (f 0)))
                 (func (f qq) : (ifleq0? qq qq (+ qq 1))))) 14)


; interp-fns tests
(define interp-test1 (parse-prog '{{func {f x} : {+ x 14}}
                            {func {main} : {f 2}}}))

(define interp-test2 (parse-prog '{{func {f x} : {+ x x}}
                            {func {main} : {f 2}}}))

(define interp-test3 (parse-prog '{{func {f x} : {+ {* x 14} 2}}
    {func {main} : {f 2}}})) 

(check-equal? (interp-fns interp-test1) 16)

(check-equal? (interp-fns interp-test2) 4)

(check-equal? (interp-fns interp-test3) 30)


; interp tests
(check-equal? (interp (parse '{sub-ten{add-one 5}}) funcList ) -4)

(check-equal? (interp (parse '{square {double 2} }) funcList) 16)

(check-equal? (intexrp (NumC 42) '()) 42 "Interpreting NumC failed.")

(check-exn (regexp (regexp-quote "OAZO shouldn't get here"))(lambda () (interp (IdC 'x) '())))

(check-equal? (interp (AppC 'add-one (list (NumC 9))) fds)10 "Interpreting AppC 'add-one failed.")

(check-equal? (interp (AppC 'add-one (list (NumC 9))) fds) 10 "Interpreting AppC failed.")

(check-equal? (interp (binopC '+ (NumC 2) (NumC 3)) '()) 5 "Interpreting binopC failed.")

(check-equal? (interp (ifleq0? (NumC -1) (NumC 10) (NumC 20)) '()) 10 "Interpreting ifleq0? (true branch) failed.")

(check-equal? (interp (ifleq0? (NumC 1) (NumC 10) (NumC 20)) '()) 20 "Interpreting ifleq0? (false branch) failed.")

(check-exn (regexp (regexp-quote "OAZO failed: #(struct:binopC % #(struct:NumC 3) #(struct:NumC 3)) is invalid"))
           (lambda () (interp (binopC '% (NumC 3) (NumC 3)) '()))) ; test binopHash error

(check-exn (regexp
            (regexp-quote "interp: OAZO failed: can't divide by 0"))
           (lambda () (interp (binopC '/ (NumC 1) (NumC 0)) '())))

(check-equal? (interp (binopC '/ (NumC 6) (NumC 3)) fds) 2)


; parse-prog tests
(check-exn (regexp
            (regexp-quote "parse-prog: OAZO failed: 1 is an invalid program"))
           (lambda () (parse-prog 1)))

(check-equal? (parse-prog '{{func {f x} : {+ x 14}}
                            {func {main} : {f 2}}}) (list (FunDefC 'f '(x) (binopC '+ (IdC 'x) (NumC 14)))
                                                               (FunDefC 'main '() (AppC 'f (list (NumC 2))))))


; parse-fundef tests
(check-equal? (parse-fundef '{func {double x} : {+ x x}}) double)

(check-equal? (parse-fundef '{func {f x} : {+ x 14}}) (FunDefC 'f '(x) (binopC '+ (IdC 'x) (NumC 14))))

(check-exn (regexp
            (regexp-quote "parse-fundef: OAZO failed: (ISA President) is invalid"))
           (lambda () (parse-fundef '{ISA President})))

;helpme123
(check-exn (regexp
            (regexp-quote "parse-fundef: OAZO failed: Function name cannot be +"))
           (lambda () (parse-fundef '(func (+ x) : 13))))

;(check-exn (regexp
;            (regexp-quote "parse-fundef: OAZO failed: Function argument cannot be +"))
;           (lambda () (parse-fundef '(func (X +) : 13))))

(check-equal? (parse-fundef '{func {f} : {+ 2 2}}) (FunDefC 'f '() (binopC '+ (NumC 2) (NumC 2)))) 
(check-equal? (parse-fundef '{func {f arg1 arg2 arg3} : {+ 2 2}})
              (FunDefC 'f '(arg1 arg2 arg3) (binopC '+ (NumC 2) (NumC 2))))
(check-exn (regexp
            (regexp-quote "parse-fundef: OAZO failed: Duplicate function arguments (a a a)"))
           (lambda () (parse-fundef '{func {f a a a} : {+ 2 2}})))

(check-exn (regexp
            (regexp-quote "parse-fundef: OAZO failed: Duplicate function arguments (a a a)"))
           (lambda () (parse-fundef '{func {f a a a} : {+ 2 2}}))) 

; parse tests

(check-exn (regexp
            (regexp-quote "parse: OAZO failed: #f is invalid"))
           (lambda () (parse #f)))

(check-equal? (parse '{- {+ 4 2 } 3}) (binopC '- (binopC '+ (NumC 4) (NumC 2)) (NumC 3)))

(check-equal? (parse '{/ {* 4 2 } 3}) (binopC '/ (binopC '* (NumC 4) (NumC 2)) (NumC 3)))

(check-equal? (parse 'fasd) (IdC 'fasd))

(check-equal? (parse '{ifleq0? 5 4 2}) (ifleq0? (NumC 5) (NumC 4) (NumC 2)))

(check-exn (regexp
            (regexp-quote "parse: OAZO failed: / is invalid IdC"))
           (lambda () (parse '(+ / 3)))) 

(check-equal? (subst (NumC 1) 'x (IdC 'y)) (IdC 'y))


(check-exn (regexp
            (regexp-quote "parse: OAZO failed: (+ b) is invalid IdC"))
           (lambda () (parse '(+ b))))


; Test substitution in a complex expression involving function application and binary operations
(check-equal? 
 (subst (NumC 5) 'x ; Substitute 'x with 5
        (binopC '+ (AppC 'double (list (binopC '* (IdC 'x) (NumC 2)))) (NumC 3))) ; Original expression
 (binopC '+ (AppC 'double (list (binopC '* (NumC 5) (NumC 2)))) (NumC 3)) ; Expected result after substitution
 "Test case for substitution within nested function application and binopC failed.")


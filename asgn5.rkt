#lang typed/racket

(require typed/rackunit)

; Finished Asgn5, all test cases passed

; ExprC definitions
(define-type ExprC (U NumC IdC StrC IfC LetC AnonC AppC))
(struct NumC ([n : Real]) #:transparent)         ; Number 
(struct IdC ([s : Symbol]) #:transparent)        ; Id
(struct StrC ([s : String]) #:transparent)       ; String
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent )    ; If-else statement
(struct LetC ([names : (Listof Symbol)] [defs : (Listof ExprC)] [expr : ExprC]) #:transparent)    ; let Local vars
(struct AnonC ([args : (Listof Symbol)] [exp : ExprC]) #:transparent)    ; Anonymous function 
(struct AppC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)    ; Function Application


; Environment definitions, from book
(define-type Env [Listof Binding])
(struct Binding ([name : Symbol] [val : ValV]) #:transparent)
(define mt-env '())
(define extend-env cons)


; Value definitions 
(define-type ValV (U NumV BoolV StrV CloV PrimV  ErrV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
; primitive operators
(struct PrimV ([op : Symbol]) #:transparent)     ; should be 
(struct ErrV ([e : (-> ValV String)]) #:transparent)          ; error 


; serialize: takes in a ValV, outputs the string of given value 
(define (serialize [v : ValV]) : String
  (match v
    [(NumV n) (~v n)]
    [(BoolV b) (match b
                  [#t "true"]
                  [#f "false"])]
    [(StrV s) (~a s)]
    [(CloV args body env ) "#<procedure>"]
    [(PrimV op) "#<primop>"]))


; parse: takes in an s-expression, returns the associated ExprC  
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)] ; num
    [(? symbol? n) (IdC n)] ; id 
    [(? string? n) (StrC n)] ; string
    [(list 'if test 'then then 'else else) (IfC (parse test) (parse then) (parse else))] ; if
    [(list 'let (list (? symbol? id) '<- expr) ... body) (LetC (cast id (Listof Symbol)) (map parse (cast expr (Listof Sexp))) (parse body))]
    ;[(list 'let (list declarations ... ) body)  (parseLet declarations body) ]
    [(list 'anon (list (? symbol? ids) ...) ': expr ) (define args (cast ids (Listof Symbol)))
     (if (check-duplicates args) (error 'parse "OAZO: Duplicate arguments ~a" args) args)
     (AnonC args (parse expr))] ; anon
    [(list exprs ...) (AppC (parse (first exprs)) (map parse (rest exprs)))] ; AppC
    [other (error 'parse "OAZO failed: ~a is invalid" s)]
    ))


; interp: takes in an ExprC and Environmnet, returns a ValV
; interprets a given expression
(define (interp [exp : ExprC] [env : Env]) : ValV
  (match exp
    [(NumC n) (NumV n)]
    [(StrC s) (StrV s)]
    [(IdC n) (lookup n env)]
    [(IfC test then else) (match (interp test env)
                            [(BoolV b)
                             (cond 
                                   [b (interp then env) ]
                                   [else (interp else env)])]
                            [else (error 'interp "OAZO is not a BoolV")])]
    ;(struct LetC ([names : (Listof Symbol)] [defs : (Listof ExprC)] [expr : ExprC]) #:transparent)    ; let Local vars
    [(LetC names defs expr)
     (define values (map (lambda ([def : ExprC]) (interp def env)) defs))
     (define new-env (extend-bindings names values env))
     (interp expr new-env)]
    [(AnonC args exp) (CloV args exp env)]
    [(AppC fun args)
     (define functionValue (interp fun env))
     (define argVals (map (lambda ([arg : ExprC]) (interp arg env)) args))
     (match functionValue 
       [(CloV args body env) (cond
                               [(= (length args) (length argVals)) (interp body (extend-bindings args argVals env))]
                               [else (error 'interp "OAZO incorrect number of arguments ~a" args)]
       )]
       [(PrimV op) ; evaluates the primitives 
        (cond
          [(not ( = (length args) 2)) (error 'interp "OAZO incorrect number of arguments ~a" a)]
          [else (eval-prim op (interp (first args) env ) (interp (second args) env))])]
  )]))


; helper to evaluate the primitives
(define (eval-prim [op : Symbol] [l : ValV] [r : ValV]) : ValV
  (match op
    ['<= (match (list l r)
           [(list (NumV a) (NumV b)) (BoolV (<= a b))])]
    ['equal? (match (list l r)
               [(list (NumV a) (NumV b)) (BoolV (= a b))])]
    ['+ (match (list l r)
          [(list (NumV a) (NumV b)) (NumV (+ a b))])]
    ['- (match (list l r)
          [(list (NumV a) (NumV b)) (NumV (- a b))])]
    ['* (match (list l r)
          [(list (NumV a) (NumV b)) (NumV (* a b))])]
    ['/ (match (list l r)
          [(list (NumV a) (NumV b))
           (if (= b 0)
               (error 'prim-op "OAZO: Division by zero")
               (NumV (/ a b)))])]
    [else (error 'eval-prim (format "OAZO: user-error ~a" op))]))


(check-equal? (eval-prim '+ (NumV 6) (NumV 4)) (NumV 10))

;looks up a symbol in the environment 
(define (lookup [for : Symbol] [env : Env]) : ValV
  (match env
    ['() (error 'lookup "name not found: ~e" for)]
    [(cons (Binding name val) r) (cond
                                   [(symbol=? for name) val] ; changed to NumV
                                   [else (lookup for r)])]))


; extends an environment 
(define (extend [arg : Symbol] [val : ValV] [env : Env]) : Env
  (extend-env (Binding arg val) env))


;extends multiple environments based on arguments, adds binding  
(define (extend-bindings [args : (Listof Symbol)] [int : (Listof ValV)] [env : Env]) : Env
  (cond
    [(empty? args) env] ; If args is empty, return the current environment
    [else ((inst foldl Symbol ValV Env) extend env args int)])) ; fold to extend environment


; Top Level Environment

; top-interp function
; Combines parsing and interpreting 
(define (top-interp [s : Sexp]) : String
  (define top-env
  (list (Binding '+ (PrimV '+))
        (Binding '- (PrimV '-))
        (Binding '* (PrimV '*))
        (Binding '/ (PrimV '/))
        (Binding '<= (PrimV '<))
        (Binding 'equal? (PrimV 'equal?))
        (Binding 'true (BoolV #t))
        (Binding 'false (BoolV #f))))
  (serialize (interp (parse s) top-env)))

(define d '{let
  {z <- {+ 9 14}}
  {y <- 98}
  {+ z y}} ; be able to parse this
)

(define a '{let
                {z <- 98}
             {+ z z }})

(define b '{{anon {z y} : {+ z y}}
 {+ 9 14}
 98})
; parse test cases

(check-equal? (top-interp d) (top-interp b))

(check-equal? (parse a) (LetC '(z) (list (NumC 98)) (AppC (IdC '+) (list (IdC 'z) (IdC 'z)))))
(check-equal? (parse '{+ 5 6}) (AppC (IdC '+) (list (NumC 5) (NumC 6))))
(check-equal? (parse '{if 5 then 2 else 6})  (IfC (NumC 5) (NumC 2) (NumC 6)))
(check-equal? (parse '{anon {z y} : {+ z y}}) (AnonC '(z y) (AppC (IdC '+) (list (IdC 'z) (IdC 'y)))))



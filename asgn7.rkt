#lang typed/racket

(require typed/rackunit)

; Assignment 7 

; ExprC definitions
(define-type ExprC (U NumC IdC StrC IfC AnonC AppC))
(struct NumC ([n : Real]) #:transparent)         ; Number 
(struct IdC ([s : Symbol]) #:transparent)        ; Id
(struct StrC ([s : String]) #:transparent)       ; String
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent )    ; If-else statement
(struct AnonC ([args : (Listof Symbol)] [exp : ExprC]) #:transparent)    ; Anonymous function 
(struct AppC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)    ; Function Application
(struct SeqC ([bodys : (Listof ExprC)]) #:transparent)


; Environment definitions, from book
(define-type-alias Location number)
(define-type Env [Listof Binding])
(struct Binding ([name : Symbol] [val : Location]) #:transparent)
(define mt-env '())
(define extend-env cons)

(struct  Storage ([(loc : Location) (val : ValV)]) #:transparent)

(define-type-alias Store (listof Storage))
(define mt-store '())
(define override-store cons)

; Value definitions 
(define-type ValV (U NumV BoolV StrV CloV PrimV ErrV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
; primitive operators
(struct PrimV ([op : Symbol]) #:transparent)     
(struct ErrV ([e : (-> ValV String)]) #:transparent)   ; error 
(struct BoxV ([l : Location]) #:transparent)  ; box 


; serialize: takes in a ValV, outputs the string of given value 
(define (serialize [v : ValV]) : String
  (match v
    [(NumV n) (~v n)]
    [(BoolV b) (match b
                  [#t "true"]
                  [#f "false"])]
    [(StrV s) (~v s)]
    [(CloV args body env ) "#<procedure>"]
    [(PrimV op) "#<primop>"]))

; Checks if idc is valid 
(define (valid? [id : Sexp]) : Boolean
  (match id
    [(or 'let 'if 'anon 'then 'else '<- ':) #f]
    [else #t]))

; checks if it is a valid let
(define (validLet? [id : (Listof Symbol)]) : Boolean
   (ormap (lambda ([s : Symbol]) (or (equal? s ':) (equal? s '<-))) id))

#;{let
  {z <- {+ 9 14}}
  {y <- 98}
  {+ z y}}

#;{{anon {z y} : {+ z y}}
 {+ 9 14}
 98}

; parse: takes in an s-expression, returns the associated ExprC  
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)] ; num
    [(? symbol? n) (cond
                    [(not (valid? n)) (error 'parse "OAZO: Invalid Idc ~a" n)]
                    [else (IdC n)])] ; id 
    [(? string? n) (StrC n)] ; string
    [(list 'if test 'then then 'else else) (IfC (parse test) (parse then) (parse else))]
    [(list 'let (list (? symbol? id) '<- expr) ... body)
     (define ids (cast id (Listof Symbol)))
     (cond
      [(validLet? ids) (error 'parse "OAZO: Invalid Ids ~a" ids)]
      [(check-duplicates ids) (error 'parse "OAZO: Invalid duplicate variable ~a" ids)]                                                         
      [else (AppC (AnonC ids (parse body)) (map parse (cast expr (Listof Sexp))))])]
    [(list 'anon (list (? symbol? ids) ...) ': expr ) (define args (cast ids (Listof Symbol)))
     (if (check-duplicates args) (error 'parse "OAZO: Duplicate arguments ~a" args) args)
     (AnonC args (parse expr))]
    [(list 'seq exprs ...) (SeqC (map parse (cast exprs (Listof Sexp))))] ; for the seq clause 
    [(list exprs ...) (AppC (parse (first exprs)) (map parse (rest exprs)))]))

(define-type Type )
(define (parse-type [s : Sexp]) : Type )
(struct v*s ([val : ValV] [sto : Store]) #: transparent) 
; interp: takes in an ExprC and Environmnet, returns a ValV
; interprets a given expression
(define (interp [exp : ExprC] [env : Env] [sto : Sto]) : v*s
  (match exp
    [(NumC n) (v*s (NumV n) sto)]
    [(StrC s) (v*s (StrV s) sto)]
    [IdC (n) (v*s (fetch (lookup n env) sto) sto)]
    [(IfC test then else) (match (interp test env)
                            [(BoolV b)
                             (cond 
                                   [b (interp then env) ] 
                                   [else (interp else env)])]
                            [else (error 'interp "OAZO is not a BoolV")])]
    [(AnonC args exp) (v*s (CloV args exp env) sto)]
    [(AppC fun args)
     (define functionValue (interp fun env))
     (define argVals (map (lambda ([arg : ExprC]) (interp arg env)) args))
     (match functionValue 
       [(CloV args body env) (cond
                               [(= (length args) (length argVals)) (interp body (extend-bindings args argVals env))]
                               [else (error 'interp "OAZO incorrect number of arguments ~a" args)]
       )]
       [(ErrV e) (error 'interp (e (interp (first args) env)))]
       [(PrimV op) ; evaluates the primitives
        (eval-prim op argVals)]
      [other (error 'interp (format "OAZO: user-error ~a" exp))]
  )]))


; eval-prim: helper to evaluate the primitives
; takes in a symbol of operation, and list of values to do the operation to, returns a value
(define (eval-prim [op : Symbol] [vals : (Listof ValV)]) : ValV
  (match op
    ['seq (last vals)] ; make it illegal to call seq with < 2 arguments 
    [(or '<= 'equal? '+ '- '* '/)
     (cond
       [(not (= (length vals) 2)) (error 'eval-prim "OAZO: incorrect number of arguments ~a" vals)]
       [else (match op
               ['<= (match (list (first vals) (second vals))
                      [(list (NumV a) (NumV b)) (BoolV (<= a b))]
                      [other (error 'eval-prim "OAZO: Invalid types given for <= operation")])]
               ['equal? (match (list (first vals) (second vals))
                          [(list (NumV a) (NumV b)) (BoolV (= a b))]
                          [(list (BoolV a) (BoolV b)) (BoolV (equal? a b))]
                          [(list (StrV a) (StrV b)) (BoolV (equal? a b))]
                          [other (BoolV #f)]
                          )]
               ['+ (match (list (first vals) (second vals))
                     [(list (NumV a) (NumV b)) (NumV (+ a b))]
                     [other (error 'eval-prim "OAZO: Invalid types given for + operation")])]
               ['- (match (list (first vals) (second vals))
                     [(list (NumV a) (NumV b)) (NumV (- a b))]
                     [other (error 'eval-prim "OAZO: Invalid types given for - operation")])]
               ['* (match (list (first vals) (second vals))
                     [(list (NumV a) (NumV b)) (NumV (* a b))]
                     [other (error 'eval-prim "OAZO: Invalid types given for * operation")])]
               ['/ (match (list (first vals) (second vals))
                     [(list (NumV a) (NumV b))
                      (if (= b 0)
                          (error 'eval-prim "OAZO: Division by zero")
                          (NumV (/ a b)))]
                     [other (error 'eval-prim "OAZO: Invalid types given for / operation")])])])]
    [other (error 'eval-prim "OAZO: Invalid operator" op) ]))



(check-equal? (eval-prim '+ (NumV 6) (NumV 4)) (NumV 10))
(check-equal? (eval-prim 'equal? (PrimV 'a) (PrimV 'd)) (BoolV #f))
(check-equal? (eval-prim 'equal? (BoolV #t) (BoolV #t)) (BoolV #t))
(check-equal? (eval-prim 'equal? (StrV "sus") (StrV "baka")) (BoolV #f))


; looks up a symbol in the environment, needs to be changed for env -> location 
(define (lookup [for : Symbol] [env : Env]) : Location ; location is a number (location)
  (match env
    ['() (error 'lookup "OAZO name not found: ~e" for)]
    [(cons (Binding name val) r) (cond
                                   [(symbol=? for name) val] ; changed to NumV
                                   [else (lookup for r)])]))


;(struct  Storage ([(loc : Location) (val : ValV)]) #:transparent)

;(define-type-alias Store (listof Storage))
; fetch returns a location (number) 
(define (fetch [for : Location] [sto : Store]) : ValV
  (match sto
    ['() (error 'fetch "OAZO location not found: ~e" loc)]
    [(cons (Storage loc val) r) (cond 
      [(equals? for loc ) val]
      [else (fetch for r)])])) 

; extends an environment 
(define (extend [arg : Symbol] [val : ValV] [env : Env]) : Env
  (extend-env (Binding arg val) env))


; extends multiple environments based on arguments, adds binding  
(define (extend-bindings [args : (Listof Symbol)] [vals : (Listof ValV)] [env : Env]) : Env
  ((inst foldl Symbol ValV Env)
   (lambda (arg val env) (extend arg val env))
   env
   args
   vals))


; Top Level Environment
; top-interp: takes in a s-expression, returns a string
(define (top-interp [s : Sexp]) : String
  (define top-env
  (list (Binding '+ (PrimV '+))
        (Binding '- (PrimV '-))
        (Binding '* (PrimV '*))
        (Binding '/ (PrimV '/))
        (Binding '<= (PrimV '<=))
        (Binding 'equal? (PrimV 'equal?))
        (Binding 'true (BoolV #t))
        (Binding 'error (ErrV user-error))
        (Binding 'false (BoolV #f))))
        (Binding 'arr ) ; todo 
        (Binding 'aref ) ; todo 
        (Binding 'aset! ) ; todo 
  (serialize (interp (parse s) top-env)))

(define (user-error [v : ValV]) : String
  (string-append "OAZO: user-error : " (serialize v)))

(define d '{let
  {z <- {+ 9 14}}
  {y <- 98}
  {+ z y}} 
)

(define a '{let
                {z <- 98}
             {+ z z }})

(define b '{{anon {z y} : {+ z y}}
 {+ 9 14}
 98})

(define test1 '{let
  {z <- {- 9 14}}
  {y <- 98}
  {b <- {* 3 14}}
  {a <- {/ 9 14}}             
  {- z b}} 
)

(define test2 '{let
  {z <- {- 9 14}}
  {y <- 98}
  {b <- {* 3 14}}
  {a <- {/ 9 14}}
  {- 1}} 
)

(define test3 '{{anon {z y} : {+ z y}}
 {+ 9 d}
 98})

(define test4 '{let
  {z <- {/ 0 0}}
  {y <- 98}
  {+ z y}} 
)

(define test5 '{let
  {z <- {0 0}}
  {y <- 98}
  {+ z y}} 
)

(define test6 '{let
  {g <- {<= 5 2}}
  {z <- {+ 5 2}}            
  {y <- 58}
  {gg <- "bsduaf"}
  {f <- {anon {z y } : {+ z y}}}
  {if {equal? 4 {f 2 4}} then {+ 4 {f 6 3}} else {- 3 {f 5 2}}}} 
)

(define test7 '{{anon {z y d} : {+ z {+ d y}}}
 {+ 4 4}
 98})

(define test8 '{let
  {g <- {<= 5 2}}
  {z <- {+ 5 2}}            
  {y <- 58}
  {gg <- "bsduaf"}
  {if z then {+ 4 3} else {- 3 2}}})

(define test9 '{let
  {g <- {<= 5 2}}
  {z <- {+ 5 2}}            
  {y <- 58}
  {gg <- "bsduaf"}
  {if {<= z 99} then {+ 4 3} else {- 3 2}}})


; -- top-interp test cases --
(check-equal? (top-interp test9) "7")
(check-exn (regexp (regexp-quote "interp: OAZO is not a BoolV"))
           (lambda () (top-interp test8)))
(check-exn (regexp (regexp-quote "interp: OAZO incorrect number of arguments (z y d)"))
           (lambda () (top-interp test7)))
(check-equal? (top-interp test6) "-4")
(check-exn (regexp (regexp-quote "interp: OAZO: user-error #(struct:AppC #(struct:NumC 0) (#(struct:NumC 0)))"))
           (lambda () (top-interp test5)))
(check-exn (regexp (regexp-quote "eval-prim: OAZO: Division by zero"))
           (lambda () (top-interp test4)))
(check-exn (regexp (regexp-quote "lookup: OAZO name not found: 'd"))
           (lambda () (top-interp test3)))
(check-exn (regexp (regexp-quote "interp: OAZO incorrect number of arguments (let (z <- 98) (+ z z))"))
           (lambda () (top-interp test2)))
(check-equal? (top-interp test1) "-47")
(check-equal? (top-interp d) (top-interp b))

(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for + operation"))
           (lambda () (top-interp '(+ + +))))
(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for * operation"))
           (lambda () (top-interp '(* * *))))
(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for / operation"))
           (lambda () (top-interp '(/ / /))))
(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for - operation"))
           (lambda () (top-interp '(- - -))))
(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for <= operation"))
           (lambda () (top-interp '(<= <= <=))))

(define t1 '{{anon {z z z} : {+ z y}}
 {+ 9 d}
 98})

; other test cases 
(check-exn (regexp (regexp-quote "parse: OAZO: Invalid duplicate variable (z z)"))
           (lambda () (parse '(let
          (z <- (anon () : 3))
          (z <- 9)
          (z)))))

(check-exn (regexp (regexp-quote "interp: OAZO: user-error : \"1234\""))
           (lambda () (top-interp '(+ 4 (error "1234")))))

(check-exn (regexp (regexp-quote "parse: OAZO: Invalid Ids (:)"))
           (lambda ()  (parse '(let (: <- "") "World"))))

;your code failed a test: (top-interp (quote (if true then "one" else "two"))) evaluated to "one", expecting "\"one\""
;Saving submission with errors.

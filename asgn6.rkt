#lang typed/racket

(require typed/rackunit)

; Finished Assignment 6 


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
(define-type ValV (U NumV BoolV StrV CloV PrimV ErrV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)                  ; primitives
(struct ErrV ([e : (-> ValV String)]) #:transparent)          ; error


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


; valid?: checks if idc is valid 
(define (valid? [id : Sexp]) : Boolean
  (match id
    [(or 'let 'if 'anon 'then 'else '<- ': ) #f]
    [else #t]))


; validLet?: checks if it is a valid let
(define (validLet? [id : (Listof Symbol)]) : Boolean
   (ormap (λ ([s : Symbol]) (or (equal? s ':) (equal? s '<-))) id))


; parse: takes in an s-expression, returns the associated ExprC  
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)] ; num
    [(? symbol? n) (cond
                    [(not (valid? n)) (error 'parse "OAZO: Invalid Idc ~a" n)]
                    [else (IdC n)])] ; id 
    [(? string? n) (StrC n)] ; string
    [(list 'if test 'then then 'else else) (IfC (parse test) (parse then) (parse else))] ; if
    [(list 'let (list (? symbol? id) '<- expr) ... body)
     (define ids (cast id (Listof Symbol)))
     (cond
      [(validLet? ids) (error 'parse "OAZO: Invalid Ids ~a" ids)]
      [(check-duplicates ids) (error 'parse "OAZO: Invalid duplicate variable ~a" ids)]                                                         
      [else (LetC ids (map parse (cast expr (Listof Sexp))) (parse body))])]
    [(list 'anon (list (? symbol? ids) ...) ': expr ) (define args (cast ids (Listof Symbol)))
     (if (check-duplicates args) (error 'parse "OAZO: Duplicate arguments ~a" args) args)
     (AnonC args (parse expr))] ; anon
    [(list exprs ...) (AppC (parse (first exprs)) (map parse (rest exprs)))] ; AppC
    ))


; interp: takes in an ExprC and Environment, returns a ValV 
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
    [(LetC names defs expr)
     (define values (map (λ ([def : ExprC]) (interp def env)) defs))
     (define new-env (extend-bindings names values env))
     (interp expr new-env)]
    [(AnonC args exp) (CloV args exp env)]
    [(AppC fun args)
     (define functionValue (interp fun env))
     (define argVals (map (λ ([arg : ExprC]) (interp arg env)) args))
     (match functionValue 
       [(CloV args body env) (cond
                               [(= (length args) (length argVals)) (interp body (extend-bindings args argVals env))]
                               [else (error 'interp "OAZO incorrect number of arguments ~a" args)]
       )]
       [(ErrV e) (error 'interp (e (interp (first args) env)))]
       [(PrimV op) ; evaluates the primitives
        (eval-prim op argVals)]
       [other (error 'interp (format "OAZO: invalid operator"))])]
      [other (error 'interp (format "OAZO: user-error ~a" exp))]))


; coerceString: helper to coerce ValV into string types
(define (coerceString [v : ValV]) : String
  (match v
    [(NumV n) (number->string n)] 
    [(StrV s) s]))


; eval-prim: helper to evaluate the primitives
(define (eval-prim [op : Symbol] [vals : (Listof ValV)]) : ValV
  (match op
    ['println (cond
                [(not (= (length vals) 1)) (error 'eval-prim "OAZO: println can only have 1 argument")]
                [else (match (first vals)
                        [(StrV s) (printf "~v\n" s) (BoolV #t)])])]
    ['read-num (printf ">") (let ([input (read)])
               (cond
                 [(real? input) (NumV input)]
                 [else (NumV -1)]))]
    ['++ (StrV (string-append* (map (λ ([val : ValV])  (coerceString val)) vals)))]
    ['seq (last vals)]
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


; looks up a symbol in the environment 
(define (lookup [for : Symbol] [env : Env]) : ValV
  (match env
    ['() (error 'lookup "OAZO name not found: ~e" for)]
    [(cons (Binding name val) r) (cond
                                   [(symbol=? for name) val] ; changed to NumV
                                   [else (lookup for r)])]))


; extends an environment 
(define (extend [arg : Symbol] [val : ValV] [env : Env]) : Env
  (extend-env (Binding arg val) env))


; extends multiple environments based on arguments, adds binding  
(define (extend-bindings [args : (Listof Symbol)] [vals : (Listof ValV)] [env : Env]) : Env
  ((inst foldl Symbol ValV Env)
   (λ (arg val env) (extend arg val env))
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
        (Binding 'println (PrimV 'println))
        (Binding 'read-num (PrimV 'read-num))
        (Binding 'seq (PrimV 'seq))
        (Binding '++ (PrimV '++))
        (Binding 'true (BoolV #t))
        (Binding 'error (ErrV user-error))
        (Binding 'false (BoolV #f))))
  (serialize (interp (parse s) top-env)))


; returns a user error itself 
(define (user-error [v : ValV]) : String
  (string-append "OAZO: user-error : " (serialize v)))


; -- eval-prim test cases --
(check-equal? (eval-prim '+ (list (NumV 6) (NumV 4))) (NumV 10))
(check-equal? (eval-prim 'equal? (list(PrimV 'a) (PrimV 'd))) (BoolV #f))
(check-equal? (eval-prim 'equal? (list(BoolV #t) (BoolV #t))) (BoolV #t))
(check-equal? (eval-prim 'equal? (list(StrV "sus") (StrV "baka"))) (BoolV #f))

; -- Interp test cases TO DO -- 
(check-exn (regexp (regexp-quote "interp: OAZO: user-error : \"1234\""))
           (λ () (top-interp '(+ 4 (error "1234")))))

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
(check-exn (regexp (regexp-quote "eval-prim: OAZO: Division by zero"))
           (lambda () (top-interp test4)))
(check-exn (regexp (regexp-quote "lookup: OAZO name not found: 'd"))
           (lambda () (top-interp test3)))
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


; -- parse test cases --
(check-equal? (parse "abcdefg") (StrC "abcdefg")) 
(check-exn (regexp (regexp-quote "parse: OAZO: Duplicate arguments (z z z)"))
           (lambda () (top-interp t1)))
(check-exn (regexp (regexp-quote "parse: OAZO: Invalid Idc anon"))
           (lambda () (parse '{anon : })))
(check-equal? (parse a) (LetC '(z) (list (NumC 98)) (AppC (IdC '+) (list (IdC 'z) (IdC 'z)))))
(check-equal? (parse '{+ 5 6}) (AppC (IdC '+) (list (NumC 5) (NumC 6))))
(check-equal? (parse '{if 5 then 2 else 6})  (IfC (NumC 5) (NumC 2) (NumC 6)))
(check-equal? (parse '{anon {z y} : {+ z y}}) (AnonC '(z y) (AppC (IdC '+) (list (IdC 'z) (IdC 'y)))))


; -- serialize test cases --
(check-equal? (serialize (NumV 5)) "5")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "hello")) "\"hello\"")
(check-equal? (serialize (CloV '() (NumC 5) mt-env)) "#<procedure>")
(check-equal? (serialize (PrimV '+)) "#<primop>")


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

; -- OAZO6 program --
; Gives the player 5 guesses, allows player to keep playing if the player does not guess the correct
; CSC course number. 
#;(top-interp '{let {target <- 430}
               {seq
                {println "Let's play a guessing game! Guess the course number of the best CSC course!"}
                {let {game <- {anon {guesses game2} :
                                    {if {<= 5 guesses}
                                        then
                                        {seq
                                         {println "Would you like to play again? [1 For Yes] [0 For No]"}
                                         {let {playAgain <- {read-num}}
                                           {if {equal? playAgain 0}
                                               then {println "Thank you for playing!"}
                                               else {seq
                                                     {println "Playing again!"}
                                                     {game2 0 game2}}
                                               }}
                                         }
                                        else
                                        {let {guess <- {read-num}}
                                          {if {equal? guess target}
                                              then {println {++ "You got it right " target}}
                                              else
                                              {seq
                                               {println {++ "Wrong answer, " {- 4 guesses} " guesses remaining!"}}
                                               {game2 {+ guesses 1 } game2}}
                                              }}
                                        }}}
                  {game 0 game}}
}}) 
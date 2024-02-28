#lang typed/racket

(require typed/rackunit)

; Started Asgn6

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


; Checks if idc is valid 
(define (valid? [id : Sexp]) : Boolean
  (match id
    [(or 'let 'if 'anon 'then 'else '<- ': ) #f]
    [else #t]))


; checks if it is a valid let
(define (validLet? [id : (Listof Symbol)]) : Boolean
   (ormap (lambda ([s : Symbol]) (or (equal? s ':) (equal? s '<-))) id))


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
       [(ErrV e) (error 'interp (e (interp (first args) env)))]
       [(PrimV op) ; evaluates the primitives ***UPDATE***
        (eval-prim op argVals)])]
      [other (error 'interp (format "OAZO: user-error ~a" exp))]))


; helper to evaluate ++ procedure 
(define (join-helper [lst : (Listof ValV)] ) : String
  (match lst
    ['() ""]
    [(cons f r) (string-append (coerceString f) (join-helper r))]))

; helper to coerce ValV into string types
(define (coerceString [v : ValV]) : String
  (match v
    [(NumV n) (number->string n)] 
    [(StrV s) s]
    ))

; helper to evaluate the primitives ***UPDATE***
(define (eval-prim [op : Symbol] [vals : (Listof ValV)]) : ValV
  (match op
    ['println (cond
                [(not (= (length vals) 1)) (error 'eval-prim "OAZO: println can only have 1 argument")]
                [else (match (first vals)
                        [(StrV s) (printf "~v\n" s) (BoolV #t)])])]
    ['read-num (printf ">") (let ([input (read)])
               (cond
                 [(real? input) (NumV input)]
                 [else (error 'eval-prim "OAZO: Input NaN ~a" input)]))]
    ['++ (StrV (join-helper vals))]
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
                     [other (error 'eval-prim "OAZO: Invalid types given for / operation")])])])]))


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

(check-exn (regexp (regexp-quote "interp: OAZO: user-error : \"1234\""))
           (lambda () (top-interp '(+ 4 (error "1234")))))

(top-interp '{let {your-number <- {read-num}}
               {println {++ "Interesting. You picked " your-number ". good choice!"}}}) ; returns "true"


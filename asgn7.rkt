#lang typed/racket

(require typed/rackunit)

; Assignment 7, missing 4 test cases

; Expression definitions
(define-type ExprC (U NumC IdC StrC IfC AnonC AppC SeqC AssignC))
(struct NumC ([n : Real]) #:transparent)         ; Number 
(struct IdC ([s : Symbol]) #:transparent)        ; Id
(struct StrC ([s : String]) #:transparent)       ; String
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent )    ; If-else statement
(struct AppC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)    ; Function Application
(struct SeqC ([bodys : (Listof ExprC)]) #:transparent)
(struct AssignC ([id : Symbol] [val : ExprC]) #:transparent)
(struct AnonC ([args : (Listof Symbol)] [body : ExprC] [arg-types : (Listof Type)])#:transparent)

; Value + Store
(struct v*s ([val : ValV] [sto : Store]) #:transparent) 


; Value definitions 
(define-type ValV (U NumV BoolV StrV CloV PrimV ArrV NullV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)     
(struct ArrV ([loc : Real] [size : Real]) #:transparent)  
(struct NullV () #:transparent)


; Type Definitions 
(define-type Type (U NumT StrT BoolT FunT ArrT VoidT))
(struct NumT () #:transparent)
(struct StrT () #:transparent)
(struct BoolT () #:transparent)
(struct FunT ([argTypes : (Listof Type)] [result : Type]) #:transparent)
(struct ArrT () #:transparent)
(struct VoidT () #:transparent)


; Environment definition
(define-type Env [Listof Binding])
(struct Binding ([name : Symbol] [loc : Real]) #:transparent) 
(define mt-env '())
(define extend-env cons)


; Storage definition 
(struct Storage ([loc : Real] [val : ValV]) #:transparent)
(define-type Store (Listof Storage))
(define mt-store '())
(define override-store cons)


; Top Environment
(define top-env
  (list (Binding '+ 1)
        (Binding '- 2)
        (Binding '* 3)
        (Binding '/ 4)
        (Binding '<= 5)
        (Binding 'num-eq? 6)
        (Binding 'str-eq? 7)
        (Binding 'arr-eq? 8)
        (Binding 'true 9)
        (Binding 'false 10)
        (Binding 'arr 11) ; todo 
        (Binding 'aref 12) ; todo 
        (Binding 'aset 13)
        (Binding 'alen 14)
        (Binding 'substring 15))) ; todo     


; Top Store
(define top-store 
  (list (Storage 1 (PrimV '+))
        (Storage 2 (PrimV '-))
        (Storage 3 (PrimV '*))
        (Storage 4 (PrimV '/))
        (Storage 5 (PrimV '<=))
        (Storage 6 (PrimV 'num-eq?))
        (Storage 7 (PrimV 'str-eq?))
        (Storage 8 (PrimV 'arr-eq?))
        (Storage 9 (BoolV #t))
        (Storage 10 (BoolV #f))
        (Storage 11 (PrimV 'arr))
        (Storage 12 (PrimV 'aref))
        (Storage 13 (PrimV 'aset))
        (Storage 14 (PrimV 'alen))
        (Storage 15 (PrimV 'substring))))
  


; Type Environment definition 
(define-type TypeEnv (Listof TypeBind))
(struct TypeBind [(name : Symbol) (type : Type)] #:transparent)


; Top Type Environment
(define base-tenv
  (list (TypeBind '+ (FunT (list (NumT) (NumT)) (NumT)))
        (TypeBind '- (FunT (list (NumT) (NumT)) (NumT)))
        (TypeBind '* (FunT (list (NumT) (NumT)) (NumT)))
        (TypeBind '/ (FunT (list (NumT) (NumT)) (NumT)))
        (TypeBind '<= (FunT (list (NumT) (NumT)) (BoolT)))
        (TypeBind 'num-eq? (FunT (list (NumT) (NumT)) (BoolT)))
        (TypeBind 'str-eq? (FunT (list (StrT) (StrT)) (BoolT)))
        (TypeBind 'arr-eq? (FunT (list (ArrT) (ArrT)) (BoolT)))
        (TypeBind 'true (BoolT))
        (TypeBind 'false (BoolT))
        (TypeBind 'arr (FunT (list (NumT) (NumT)) (ArrT)))
        (TypeBind 'aref (FunT (list (ArrT) (NumT)) (NumT)))
        (TypeBind 'aset (FunT (list (ArrT) (NumT) (NumT)) (VoidT)))
        (TypeBind 'alen (FunT (list (ArrT)) (NumT)))
        (TypeBind 'substring (FunT (list (StrT) (NumT) (NumT)) (StrT)))))



; serialize: takes in a ValV, outputs the string of given value 
(define (serialize [v : ValV]) : String
  (match v
    [(NumV n) (~v n)]
    [(BoolV b) (match b
                  [#t "true"]
                  [#f "false"])]
    [(StrV s) (~v s)]
    [(CloV args body env ) "#<procedure>"]
    [(PrimV op) "#<primop>"]
    [(NullV) "null"]
    [(ArrV loc size) "#<array>" ]))


; valid?: Checks if idC is valid 
(define (valid? [id : Sexp]) : Boolean
  (match id
    [(or 'let 'if 'anon 'then 'else '<- ':) #f]
    [else #t]))


; validLet?: checks if list of ids has valid syntax
#;(define (validLet? [id : (Listof Symbol)]) : Boolean
   (ormap (lambda ([s : Symbol]) (or (equal? s ':) (equal? s '<-))) id))


; t-lookup: looks up the symbol in the environment, returning its type
(define (type-lookup [for : Symbol] [tenv : TypeEnv]) : Type 
  (match tenv
    ['() (error 'type-lookup "OAZO name not found: ~e" for)]
    [(cons (TypeBind name type) r) (cond
                                   [(symbol=? for name) type] 
                                   [else (type-lookup for r)])]))


(check-exn (regexp (regexp-quote "type-lookup: OAZO name not found: 'sussybaka"))
           (lambda () (type-lookup 'sussybaka base-tenv)))

; type-check: checks the type of the expression
; '(+ 4 "abc")) ; {AppC '(IdC +) (list (NumC 4) (StrC "abc"))
(define (type-check [exp : ExprC] [tenv : TypeEnv]) : Type
  (match exp
    [(NumC n) (NumT)]
    [(StrC n) (StrT)]
    [(IdC n) (type-lookup n tenv)]
    [(AssignC id val) (define val-type (type-check val tenv))
                      (cond
                        [(equal? val-type (type-lookup id tenv)) val-type]
                        [else (error 'type-check "OAZO: Invalid type ~a" val-type)])]
    [(IfC test then else) (define test-type (type-check test tenv))
                          (cond
                            [(equal? test-type (BoolT)) test-type]
                            [else (error 'type-check "OAZO: Invalid Test Type ~a" test)])]
    [(AppC fun args) (define fun-type (type-check fun tenv))
                     (define argument-types (map (lambda ([arg : ExprC]) (type-check arg tenv)) args))
                     (match fun-type
                       [(FunT paramTypes retType)
                        (printf "paramTypes: ~v argTypes: ~v \n" paramTypes argument-types)
                        (cond
                       [(not (equal? paramTypes argument-types)) (error 'type-check "OAZO: Not a function ~a" fun-type)]
                       [else retType]
                       )])]
    [(SeqC exps) (last (map (lambda ([exp : ExprC]) (type-check exp tenv)) exps))]
    [(AnonC args body arg-types) (FunT arg-types (type-check body (extend-tenv args arg-types tenv)))]))

(define (extend-tenv [args : (Listof Symbol)] [types : (Listof Type)] [tenv : TypeEnv]) : TypeEnv
  (if (empty? args)
      tenv
      (cons (TypeBind (first args) (first types)) (extend-tenv (rest args) (rest types) tenv))))



; parse-type: parses a type 
(define (parse-type [s : Sexp]) : Type
  (match s
    ['num (NumT)]
    ['bool (BoolT)]
    ['str (StrT)]
    ['void (VoidT)]
    [(list args ... '-> ret)
     (FunT (map parse-type (cast args (Listof Sexp))) (parse-type ret))] ; function types
    ['numarray (ArrT)]
    [other (error 'parse-type "OAZO: Invalid Type ~a" other)]))
    
; parse: takes in an s-expression, returns the associated ExprC  
(define (parse [s : Sexp]) : ExprC
  (match s
    [(list 'if test 'then then 'else else) (IfC (parse test) (parse then) (parse else))]
    [(list 'let (? list? l (list (list (? symbol? arg) ': type) '<- expr )) ... body)
     (define types (cast type (Listof Sexp)))
     (define args (cast arg (Listof Symbol)))
     (cond
      ;[(validLet? args) (error 'parse "OAZO: Invalid Arguments ~a" args)]
      [(check-duplicates args) (error 'parse "OAZO: Duplicate arguments ~a" args)]                                                         
      [else (AppC (AnonC args (parse body) (map parse-type types)) (map parse (cast expr (Listof Sexp))))])]
    [(? real? n) (NumC n)] ; num
    [(? symbol? n) (cond
                    [(not (valid? n)) (error 'parse "OAZO: Invalid Idc ~a" n)]
                    [else (IdC n)])] ; id 
    [(? string? n) (StrC n)] ; string
    [(list (? symbol? id) ':= expr) (AssignC id (parse expr))] ; assignment
    [(list 'anon (list (list type (? symbol? id)) ...) ': expr )
     (define arg-types (cast type (Listof Sexp)))
     (define args (cast id (Listof Symbol))) ; anon
     (if (check-duplicates args) (error 'parse "OAZO: Duplicate arguments ~a" args) args)
     (AnonC args (parse expr) (map parse-type arg-types))]
    [(list 'seq body ...)
     (let ([bodys (map parse body)])
       (if (empty? bodys)
           (error 'parse "OAZO: Empty sequence")
           (SeqC bodys)))]
    [(list exprs ...) (AppC (parse (first exprs)) (map parse (rest exprs)))])) ; appc 


; interp: takes in an ExprC and Environmnet, returns a ValV
; interprets a given expression
(define (interp [exp : ExprC] [env : Env] [sto : Store]) : v*s
  (match exp
    [(NumC n) (v*s (NumV n) sto)]
    [(StrC s) (v*s (StrV s) sto)]
    [(IdC n) (v*s (fetch (lookup n env) sto) sto)]
    [(AssignC s e)
     (match (interp e env sto)
       [(v*s new-val sto2) (define loc (lookup s env))
                        (define new-store (override-store (Storage loc new-val) sto2))
                             (v*s (NullV) new-store)])]; adds a new store cons ])   
     [(IfC test then else) (match (interp test env sto)
                            [(v*s (BoolV b) sto)
                             (cond 
                                   [b (interp then env sto) ] 
                                   [else (interp else env sto)])])]
                            ;[else (error 'interp "OAZO: test is not a BoolV ~a" test)])]
    [(AnonC args exp types) (v*s (CloV args exp env) sto)]
    [(SeqC bodys) (define-values (bodyVals sto2) (interp-args bodys env sto))
     (v*s (last bodyVals) sto2)]
    [(AppC fun args)
     (match (interp fun env sto)
       [(v*s functionValue sto) (define-values (argVals sto2) (interp-args args env sto))
        (match functionValue
          [(CloV args body env) (cond
                                  [(= (length args) (length argVals))
                                   (define store-loc (next-allocated sto2))
                                   (define new-env (bind-environments args store-loc env))
                                   (define new-store (bind-store argVals store-loc sto2))
                                   (interp body new-env new-store)]
                                  [else (error 'interp "OAZO incorrect number of arguments ~a" args)])]
          [(PrimV op) ; evaluates the primitives
            (eval-prim op argVals sto2)])])]))
          ;[other (error 'interp (format "OAZO: user-error ~a" exp))])])]))


; binds the argument symbols to its respective location in the store, returning the new store  
(define (bind-environments [args : (Listof Symbol)] [loc : Real] [env : Env]) : Env
    (cond
      [(empty? args) env]
      [else
       (bind-environments (rest args) (+ loc 1) (cons (Binding (first args) loc) env))]))


;(bind-store: binds the list of arguments values to the store, returning new store  
(define (bind-store [argVals : (Listof ValV)] [loc : Real] [sto : Store]) : Store
    (cond
      [(empty? argVals) sto]
      [else
       (bind-store (rest argVals) (+ loc 1) (cons (Storage loc (first argVals)) sto))]))


; Interprets the arguments, extending the store and returns the args and extended store 
(define (interp-args [args : (Listof ExprC)] [env : Env] [sto : Store]) : (Values (Listof ValV) Store)
  (match args
    ['() (values '() sto)]
    [(cons arg rest)
     (match (interp arg env sto)
       [(v*s arg-val sto2)
        (define-values (rest-vals final-sto) (interp-args rest env sto2))
        (values (cons arg-val rest-vals) final-sto)])]))


; next-allocated: Takes in a Store (list of Storage) and returns the size of it + 1 (next allocated)
(define (next-allocated [store : Store]) : Real
  (+ 1 (length store)))


; allocate-helper: allocates to the store the Number of locations with the value val, 
; returning the base/ start of the location and the new store, used for array
(define (allocate-helper [sto : Store] [num : Real] [val : ValV]) : (Values Real Store)
  (define next-loc (next-allocated sto))
  (define new-locs (build-list (cast num Integer) (lambda ([x : Real]) (+ next-loc x))))
  (define new-store (foldl (lambda ([loc : Real] [store : Store]) (cons (Storage loc val) store)) sto new-locs))
  (values next-loc new-store))


; go into the store until idx location matches 
(define (get-index [idx-loc : Real] [sto : Store]) : ValV
  (match sto 
    ['() (error 'aref "Index out of bounds ~a" idx-loc)]
    [(cons (Storage loc val) r) 
    (cond 
      [(= loc idx-loc) val]
      [else (get-index idx-loc r)])]))


(check-exn (regexp (regexp-quote "aref: Index out of bounds 50"))
           (lambda () (get-index 50 top-store)))
; set index should add a new binding with the same location 
; sets the location in storage to a new value
(define (aset-arr-helper [vals : (Listof ValV)] [sto : Store]) : v*s
  (match vals
    [(list (ArrV loc size) (NumV idx) new-val)
     (define idx-loc (+ idx loc))
     (cond 
      [(<= size idx) (error 'aset "Index out of bounds ~a" idx)]
      [else (define sto2 (cons (Storage idx-loc new-val) sto)) (v*s (NullV) sto2)])]))


; eval-prim: helper to evaluate the primitives
; takes in a symbol of operation, and list of values to do the operation to, returns a value
(define (eval-prim [op : Symbol] [vals : (Listof ValV)] [sto : Store]) : v*s
  (match op
    ['arr (match vals
            [(list (NumV size) (NumV default))
             (let-values ([(loc new-store) (allocate-helper sto size (NumV default))])
               (v*s (ArrV loc size) new-store))])]
    ['aref (match vals 
            [(list (ArrV loc size) (NumV idx))       
              (define idx-loc (+ loc idx)) 
              (v*s (get-index idx-loc sto) sto)])] ; make a new binding
    ['aset (aset-arr-helper vals sto)] ;(match vals 
    ['alen (match vals
            [(list (ArrV loc size)) (v*s (NumV size) sto)])]
    ['substring (match vals 
                  [(list (StrV str) (NumV start) (NumV end)) 
                  (define sub (substring str (cast start Integer) (cast end Integer)))
                  (v*s (StrV sub) sto)])]
    [(or '<=  '+ '- '* '/ 'num-eq? 'str-eq? 'arr-eq? )
     (cond
       [(not (= (length vals) 2)) (error 'eval-prim "OAZO: incorrect number of arguments ~a" vals)]
       [else (match op
               ['num-eq? (match (list (first vals) (second vals))
                      [(list (NumV a) (NumV b)) (v*s (BoolV (equal? a b)) sto)]
                      [other (error 'eval-prim "OAZO: Invalid types given for num-eq? operation")])]
               ['str-eq? (match (list (first vals) (second vals))
                      [(list (StrV a) (StrV b)) (v*s (BoolV (equal? a b)) sto)]
                      [other (error 'eval-prim "OAZO: Invalid types given for str-eq? operation")])]
               ['arr-eq? (match (list (first vals) (second vals))
                      [(list (ArrV loc1 size1) (ArrV loc2 size2)) (v*s (BoolV (and (= loc1 loc2) (= size1 size2))) sto)]
                      [other (error 'eval-prim "OAZO: Invalid types given for arr-eq? operation")])]
               ['<= (match (list (first vals) (second vals))
                      [(list (NumV a) (NumV b)) (v*s (BoolV (<= a b)) sto)]
                      [other (error 'eval-prim "OAZO: Invalid types given for <= operation")])]
               ['+ (match (list (first vals) (second vals))
                     [(list (NumV a) (NumV b)) (v*s (NumV (+ a b)) sto)]
                     [other (error 'eval-prim "OAZO: Invalid types given for + operation")])]
               ['- (match (list (first vals) (second vals))
                     [(list (NumV a) (NumV b)) (v*s (NumV (- a b)) sto)]
                     [other (error 'eval-prim "OAZO: Invalid types given for - operation")])]
               ['* (match (list (first vals) (second vals))
                     [(list (NumV a) (NumV b)) (v*s (NumV (* a b)) sto)]
                     [other (error 'eval-prim "OAZO: Invalid types given for * operation")])]
               ['/ (match (list (first vals) (second vals))
                     [(list (NumV a) (NumV b))
                      (if (= b 0)
                          (error 'eval-prim "OAZO: Division by zero")
                          (v*s (NumV (/ a b)) sto))]
                     [other (error 'eval-prim "OAZO: Invalid types given for / operation")])])])]))
    ;[other (error 'eval-prim "OAZO: Invalid operator" op) ]))

; lookup: looks up the symbol in the environment, returning its location 
(define (lookup [for : Symbol] [env : Env]) : Real 
  (match env
    ['() (error 'lookup "OAZO name not found: ~e" for)]
    [(cons (Binding name val) r) (cond
                                   [(symbol=? for name) val] 
                                   [else (lookup for r)])]))


; fetch: takes in a Real (location) and Store, returns the value in the store from the location
(define (fetch [for : Real] [sto : Store]) : ValV
  (match sto
    ['() (error 'fetch "OAZO location not found: ~e" for)]
    [(cons (Storage loc val) r) (cond 
      [(= for loc ) val]
      [else (fetch for r)])])) 


; top-interp: takes in a s-expression, returns the serialized string
(define (top-interp [s : Sexp]) : String
  (define parsed-ast (parse s))
  (type-check parsed-ast base-tenv)
  (serialize (v*s-val (interp (parse s) top-env top-store))))

; -- Test Cases --

; -- parse-type Test Cases --

(check-equal? (parse-type 'bool) (BoolT))
(check-equal? (parse-type 'void) (VoidT))
(check-equal? (parse-type 'num) (NumT))
(check-equal? (parse-type 'str) (StrT))
(check-equal? (parse-type '{num num -> bool}) (FunT (list (NumT) (NumT)) (BoolT)))

; -- Parse Test Cases --
(check-equal? (parse '{anon {[num a] [num b]} : {+ a b}})
              (AnonC '(a b) (AppC (IdC '+) (list (IdC 'a) (IdC 'b))) (list (NumT) (NumT))))

(define parse-test1 '{let
  {[z : num] <- {- 9 14}}         
  {- z 2}} 
)
(check-equal? (parse parse-test1)
(AppC (AnonC '(z) (AppC (IdC '-) (list (IdC 'z) (NumC 2)))
      (list (NumT))) (list (AppC (IdC '-) (list (NumC 9) (NumC 14))))))

(define parse-test2 '{let
  {[z : num] <- {- 9 14}}
  {[z : str] <- "sus"}
  {seq {z := 14}
       {/ 5 2}}}) 
(check-exn (regexp (regexp-quote "parse: OAZO: Duplicate arguments (z z)"))
           (lambda () (parse parse-test2)))

(define parse-test3 '{let
  {[z : num] <- {- let 14}}         
  {- z 2}} 
)
(check-exn (regexp (regexp-quote "parse: OAZO: Invalid Idc let"))
           (lambda () (parse parse-test3)))

(define parse-test4 '{let
  {[z : among] <- {- let 14}}         
  {- z 2}} 
)

(check-exn (regexp (regexp-quote "parse-type: OAZO: Invalid Type among"))
           (lambda () (parse parse-test4)))

; -- top-interp test cases --
(check-equal? (top-interp '{let
               {[a : numarray] <- {arr 4 0}}
               {[b : num] <- 43}
               {seq
                {aset a 2 44}
                {aref a 2}}}) "44")


(check-exn (regexp (regexp-quote "aset: Index out of bounds 15"))
           (lambda ()  (top-interp '{let
               {[a : numarray] <- {arr 2 2.5}}
               {[b : num] <- 45}
                {aset a 15 43}})))


(check-equal? (top-interp '{let
               {[a : num] <- {if {<= 5 2} then 7 else 4}}
                {+ 3 4}}) "7")

(check-equal? (top-interp '{let
               {[a : numarray] <- {arr 4 2.5}}
               {[b : num] <- 45}
                {alen a}}) "4")


(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for + operation"))
           (lambda ()  (top-interp '{let
               {[a : num] <- {+ "string" 5}}
               {[b : num] <- 45}
                {+ 5 4}}) ))

(check-exn (regexp (regexp-quote
 "eval-prim: OAZO: incorrect number of arguments (#(struct:NumV 5) #(struct:NumV 5) #(struct:NumV 5))"))
           (lambda ()  (top-interp '{let
               {[a : num] <- {+ 5 5 5}}
               {[b : num] <- 45}
                {+ 5 4}}) ))


(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for - operation"))
           (lambda ()  (top-interp '{let
               {[a : num] <- {- "string" 5}}
               {[b : num] <- 45}
                {+ 5 4}}) ))

(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for * operation"))
           (lambda ()  (top-interp '{let
               {[a : num] <- {* "string" 5}}
               {[b : num] <- 45}
                {+ 5 4}}) )) 

(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for / operation"))
           (lambda ()  (top-interp '{let
               {[a : num] <- {/ "string" 5}}
               {[b : num] <- 45}
                {+ 5 4}})))

(check-exn (regexp (regexp-quote "eval-prim: OAZO: Division by zero"))
           (lambda () (top-interp '{let
               {[a : num] <- {/ 5 0}}
               {[b : num] <- 45}
                {+ 5 4}})))

(check-exn (regexp (regexp-quote "type-check: OAZO: Not a function #(struct:NumT)"))
           (lambda () (top-interp '{let
               {[a : num] <- {/ 5 0}}
               {[b : num] <- 45}
                {5 4}})))


(check-equal? (top-interp '{let
               {[f : {num -> num}] <- {anon {[num z]} : {+ z 9}}}
               {[b : num] <- {- 2 0}}
               {[s : str] <- "bakayaro"}
               {[d : num] <- {* 4 {/ 3 2}}}
               {seq
                {b := {f 3}}
                {f b}}}) "21")

(check-exn (regexp (regexp-quote "parse: OAZO: Duplicate arguments (z z z)"))
           (lambda () (top-interp '{let
               {[f : {num -> num}] <- {anon {[num z] [num z] [num z]} : {+ z 9}}}
               {[b : num] <- {- 2 0}}
               {[s : str] <- "bakayaro"}
               {[d : num] <- {* 4 {/ 3 2}}}
               {seq
                {b := {f 3}}
                {f b}}})))

(check-exn (regexp (regexp-quote "parse: OAZO: Empty sequence"))
           (lambda () (top-interp '{let
               {[f : {num -> num}] <- {anon {[num z] [num z] [num z]} : {+ z 9}}}
               {seq}})))

(check-exn (regexp (regexp-quote "type-check: OAZO: Invalid type #(struct:StrT)"))
           (lambda () (top-interp '{let
               {[b : num] <- {- 2 0}}
               {seq
                {b := "string"}
                {+ 2 3}}})))

(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for num-eq? operation"))
           (lambda ()  (top-interp '{let
               {[a : bool] <- {num-eq? 5 "string"}}
               {[ff : str] <- {substring "sdfousabd" 1 2}}
               {[b : num] <- 45}
                {+ 5 4}})))

(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for str-eq? operation"))
           (lambda ()  (top-interp '{let
               {[a : bool] <- {str-eq? 5 "string"}}
               {[b : num] <- 45}
                {+ 5 4}})))

(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for arr-eq? operation"))
           (lambda ()  (top-interp '{let
               {[a : bool] <- {arr-eq? 5 "string"}}
               {[b : num] <- 45}
                {+ 5 4}})))

(check-exn (regexp (regexp-quote "eval-prim: OAZO: Invalid types given for <= operation"))
           (lambda ()  (top-interp '{let
               {[a : bool] <- {<= 5 "string"}}
               {[b : num] <- 45}
                {+ 5 4}})))


(check-exn (regexp (regexp-quote
 "type-check: OAZO: Invalid Test Type #(struct:AppC #(struct:IdC -) (#(struct:NumC 5) #(struct:NumC 3)))"))
           (lambda ()  (top-interp '{let
               {[a : num] <- {if {- 5 3} then 5 else 2}}
               {[b : num] <- 45}
                {+ 5 4}})))


(check-equal? (top-interp '{let
               {[a : bool] <- {num-eq? 5 5}}
               {[b : num] <- 45}
                {+ 5 4}}) "9")


(check-equal? (top-interp '{let
               {[d : numarray] <- {arr 2 3}}
               {[a : num] <- 5}
               {[b : num] <- 45}
                {arr-eq? d d}}) "true")

(check-equal? (top-interp '{let
               {[a : bool] <- {str-eq? "s" "s"}}
               {[b : num] <- 45}
                {+ 5 4}}) "9")

(check-equal? (top-interp '{let
               {[a : bool] <- {<= 5 6}}
               {[ff : num] <- {if {<= 5 6} then 3 else 2}}
               {[ff2 : num] <- {if {<= 6 2} then 3 else 2}}
               {[b : num] <- 45}
                {+ 5 4}}) "9")

(check-exn (regexp (regexp-quote "interp: OAZO incorrect number of arguments (a b c)"))
           (lambda () (top-interp '{let
               {[f : {num -> num}] <- {anon {[num a] [num b] [num c]} : {+ a 9}}}
               {[b : num] <- {- 2 0}}
               {[s : str] <- "bakayaro"}
               {[d : num] <- {* 4 {/ 3 2}}}
               {seq
                {b := {f 3}}
                {f b}}})))

(check-equal? (top-interp '{let
               {[f : {num -> num}] <- {anon {[num z]} : {+ z 9}}}
               {[b : num] <- 4}
               {[asa : str] <- {substring "sodaufasofu" 1 2}}
               {seq
                {b := {f 3}}
                {f b}
                {b := 1}
                b}}) "1")

; -- Misc. Test Cases -- 
(check-equal? (next-allocated top-store) 16)

(check-exn (regexp (regexp-quote "fetch: OAZO location not found: 35"))
           (lambda () (fetch 35 top-store)))

(check-exn (regexp (regexp-quote "lookup: OAZO name not found: 'sussybaka"))
           (lambda () (lookup 'sussybaka top-env)))

; -- serialize test cases -- 
(check-equal? (serialize (NumV 5)) "5")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "hello")) "\"hello\"")
(check-equal? (serialize (CloV '() (NumC 5) mt-env)) "#<procedure>")
(check-equal? (serialize (PrimV '+)) "#<primop>")
(check-equal? (serialize (NullV)) "null")
(check-equal? (serialize (ArrV 10 10)) "#<array>")

(parse '(+ 4 "abc"))
(type-check (parse '(+ 4 "abc")) base-tenv)
;Testfail: while evaluating (top-interp (quote (let ((halt : num) <- 1) ((memory : numarray) <- (arr 1000 0)) ((pc : numarray) <- (arr 1 0)) ((incr : (numarray -> num)) <- (anon ((numarray arr)) : (seq (aset arr 0 (+ 1 (aref arr 0))) 99999))) (let ((go : (-> num)) <- (let ((go : (-> num)) <- (anon () : 1234)) (seq (go := (anon () : (let ((opcode : num) <- (aref memory (aref pc 0))) (if (num-eq? opcode 0) then (seq (incr pc) (go)) else (if (num-eq? opcode 1) then (aref pc 0) else (/ 1 0)))))) go))) (seq (aset memory 453 halt) (go)))))):
;Testfail: expected exception with message containing OAZO on test expression: '(type-check (parse '(+ 4 "abc")) base-tenv)
;Testfail: expected exception with message containing OAZO on test expression: '(type-check (parse '((anon ((num a) (num b) (num c)) : 14) 3 4)) base-tenv)
;Testfail: expected exception with message containing OAZO on test expression: '(type-check (parse '((anon ((num a) (str b) (num c)) : 14) 3 4 5)) base-tenv)

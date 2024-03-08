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
(struct AssignC ([id : Symbol] [val : ExprC]) #:transparent)

; Environment definitions, from book
(define-type Env [Listof Binding])
(struct Binding ([name : Symbol] [loc : Real]) #:transparent)
(define mt-env '())
(define extend-env cons)


; Storage definition 
(struct Storage ([loc : Real] [val : ValV]) #:transparent)
(define-type Store (Listof Storage))
(define mt-store '())
(define override-store cons)

; Value + Store 
(struct v*s ([val : ValV] [sto : Store]) #:transparent) 


; Value definitions 
(define-type ValV (U NumV BoolV StrV CloV PrimV ErrV ArrV NullV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
; primitive operators
(struct PrimV ([op : Symbol]) #:transparent)     
(struct ErrV ([e : (-> ValV String)]) #:transparent)     ; error 
(struct ArrV ([loc : NumV] [size : NumV]) #:transparent)  ; array
(struct NullV () #:transparent)


; change it to 'symbol -> location 
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
        (Binding 'error 11)
        (Binding 'arr 12) ; todo 
        (Binding 'aref 13) ; todo 
        (Binding 'aset 14)
        (Binding 'alen 15)
        (Binding 'substring 16))) ; todo     


; change it to location -> value 
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
        (Storage 11 (ErrV user-error))
        (Storage 12 (PrimV 'arr))
        (Storage 13 (PrimV 'aref))
        (Storage 14 (PrimV 'aset))
        (Storage 15 (PrimV 'alen))
        (Storage 16 (PrimV 'substring))))
  

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


; Checks if idc is valid 
(define (valid? [id : Sexp]) : Boolean
  (match id
    [(or 'let 'if 'anon 'then 'else '<- ':) #f]
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
    [(list (? symbol? id) ':= expr) (AssignC id (parse expr))] ; assignment 
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
    [(list 'seq exprs ...) (SeqC (map parse exprs))] ; for the seq clause 
    [(list exprs ...) (AppC (parse (first exprs)) (map parse (rest exprs)))]))

; assign helper 
;(define (assign [s : Symbol] [s : ExprC] ) : NullV)
  ;(define ; extend env and the store )

; interp: takes in an ExprC and Environmnet, returns a ValV
; interprets a given expression
(define (interp [exp : ExprC] [env : Env] [sto : Sto]) : v*s
  (match exp
    [(NumC n) (v*s (NumV n) sto)]
    [(StrC s) (v*s (StrV s) sto)]
    [(IdC n) (v*s (fetch (lookup n env) sto) sto)]
    [(AssignC s e)
     (match (interp e env sto)
       [(v*s new-val sto2) (define loc (lookup s env))
                        (define new-store (override-store sto2 (Storage loc new-val)))
                             (v*s (NullV) new-store)])]; adds a new store cons ])   
     [(IfC test then else) (match (interp test env sto)
                            [(v*s (BoolV b) sto)
                             (cond 
                                   [b (interp then env sto) ] 
                                   [else (interp else env sto)])]
                            [else (error 'interp "OAZO: test is not a BoolV ~a" test)])]
    [(AnonC args exp) (v*s (CloV args exp env) sto)]
    [(SeqC bodys) (define-values (bodyVals _) (interp-args bodys env sto))
     (last bodyVals)]
    [(AppC fun args)
     (match (interp fun env sto)
       [(v*s functionValue sto) (define-values (argVals sto2) (interp-args args env sto))
        (match functionValue
          [(CloV args body env) (cond
                                  [(= (length args) (length argVals)) (interp body (extend-bindings args argVals env) sto2)]
                                  [else (error 'interp "OAZO incorrect number of arguments ~a" args)])]
          [(ErrV e) (error 'interp (e (interp (first args) env sto2)))]
          [(PrimV op) ; evaluates the primitives
            (eval-prim op argVals sto2)]
          [other (error 'interp (format "OAZO: user-error ~a" exp))])])]))

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
          

; allocate-helper: allocates to the store the Number of locations with the value val, returning the base/ start of the location
; and the new store, used for array
(define (allocate-helper [sto : Store] [num : Real] [val : ValV]) : (Pair Real Store)
  (define next-loc (next-allocated sto))
  (define new-locs (build-list num (lambda ([i : Real]) (+ next-loc i))))
  (define new-store (foldl (lambda (loc store) (cons (Storage loc val) store)) sto new-locs))
  (values next-loc new-store))

; go into the store until idx location matches 
(define (get-index [idx-loc : Real] [sto : Store]) : ValV
  (match sto 
    ['() (error 'aref "Index out of bounds ~a" idx-loc)]
    [(cons (Storage loc val) r) 
    (cond 
      [(= loc idx-loc) val]
      [else (get-index idx-loc r)])]))

; set index should add a new binding with the same location 
; sets the location in storage to a new value 
(define (aset-arr [arr : ArrV] [idx : Real] [newval : ValV] [sto : Store]) : v*s
  (cond 
      [(<= (ArrV-size arr) idx) (error 'aset "Index out of bounds")]
      [else(define sto2 (cons (Storage idx newval) sto)) (v*s (NullV) sto2)]))

; eval-prim: helper to evaluate the primitives
; takes in a symbol of operation, and list of values to do the operation to, returns a value
(define (eval-prim [op : Symbol] [vals : (Listof ValV)] [sto : Store]) : v*s
  (match op
    ['arr (match vals
            [(list (NumV size) (NumV default))
             (let-values ([(loc new-store) (allocate-helper sto size (NumV default))])
               (v*s (ArrV (NumV loc) (NumV size)) new-store))])]
    ['aref (match vals 
            [(list (ArrV loc size) (NumV idx))        
              (define idx-loc (+ loc idx)) 
              (v*s (get-index idx-loc sto) sto)])] ; make a new binding 
    ['aset (aset-arr (first vals) (second vals) (third vals))] ;(match vals 
            ;[(list (ArrV loc size) (NumV idx) (NumV newval)) )])] ; make a new binding 
    ['alen (match vals
            [(list (ArrV loc size)) (v*s (NumV size) sto)])]
    ['substring (match vals 
                  [(list (StrV str) (NumV start) (NumV end)) 
                  (define sub (substring str start end))
                  (v*s (StrV sub) sto)])]
    [(or '<= 'equal? '+ '- '* '/)
     (cond
       [(not (= (length vals) 2)) (error 'eval-prim "OAZO: incorrect number of arguments ~a" vals)]
       [else (match op
               ['num-eq? (match (list (first vals) (second vals))
                      [(list (NumV a) (NumV b)) (v*s (BoolV (= a b) sto))]
                      [other (error 'eval-prim "OAZO: Invalid types given for num-eq? operation")])]
               ['str-eq? (match (list (first vals) (second vals))
                      [(list (StrV a) (StrV b)) (v*s (BoolV (equal? a b) sto))]
                      [other (error 'eval-prim "OAZO: Invalid types given for str-eq? operation")])]
               ['arr-eq? (match (list (first vals) (second vals))
                      [(list (ArrV loc1 size1) (ArrV loc2 size2)) (BoolV (and (= loc1 loc2) (= size1 size2)))]
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
                     [other (error 'eval-prim "OAZO: Invalid types given for / operation")])])])]
    [other (error 'eval-prim "OAZO: Invalid operator" op) ]))


; looks up a symbol in the environment, needs to be changed for env -> location 
(define (lookup [for : Symbol] [env : Env]) : Real ; location is a number (location)
  (match env
    ['() (error 'lookup "OAZO name not found: ~e" for)]
    [(cons (Binding name val) r) (cond
                                   [(symbol=? for name) val] ; changed to NumV
                                   [else (lookup for r)])]))

; Fetch, Maps Location to Store 
(define (fetch [for : Real] [sto : Store]) : ValV
  (match sto
    ['() (error 'fetch "OAZO location not found: ~e" for)]
    [(cons (Storage loc val) r) (cond 
      [(= for loc ) val]
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
  (serialize (interp (parse s) top-env top-store)))


; user-error: sends a user error with associated value 
(define (user-error [v : ValV]) : String
  (string-append "OAZO: user-error : " (serialize v)))



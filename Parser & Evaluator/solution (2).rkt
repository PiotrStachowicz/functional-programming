#lang plait

; Gramatyka abstrakcyjna ------------------------------------------------------------------------------------------------------------------------

(define-type P
  (defE [def : (Listof D)] [b : E]))

(define-type D
  (funE [x : Symbol] [args : (Listof Symbol)] [b : E]))

(define-type E
  (numE [n : Number])
  (varE [x : Symbol])
  (opE [l : E] [op : Op] [r : E])
  (ifzE [e0 : E] [e1 : E] [e2 : E])
  (letE [x : Symbol] [e1 : E] [b : E])
  (appE [b : Symbol] [args : (Listof E)]))

(define-type Op
  (add)
  (sub)
  (mul)
  (leq))

; Parser ----------------------------------------------------------------------------------------------------------------------------------------

(define (parse-p [s : S-Exp]) : P
  (cond
    [(s-exp-match? `{define {ANY ...} for ANY} s)
     (defE (map parse-d (s-exp->list (second (s-exp->list s)))) 
       (parse-e (fourth (s-exp->list s))))]
    [else (error 'parse-p "syntax error")]))

(define (parse-d [s : S-Exp]) : D
  (cond
    [(s-exp-match? `{fun SYMBOL {ANY ...} = ANY} s)
     (funE (s-exp->symbol (second (s-exp->list s))) 
           (map (lambda (x) (s-exp->symbol x)) (s-exp->list (third (s-exp->list s)))) 
           (parse-e (fourth (rest (s-exp->list s)))))]
    [else (error 'parse-d "syntax error")]))

(define (parse-e [s : S-Exp]) : E
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [(s-exp-match? `{ANY SYMBOL ANY} s)
     (opE (parse-e (first (s-exp->list s))) 
          (parse-op (s-exp->symbol (second (s-exp->list s)))) 
          (parse-e (third (s-exp->list s))))]
    [(s-exp-match? `{ifz ANY then ANY else ANY} s)
     (ifzE (parse-e (second (s-exp->list s))) 
           (parse-e (fourth (s-exp->list s))) 
           (parse-e (fourth (rest (rest (s-exp->list s))))))]
    [(s-exp-match? `{let SYMBOL be ANY in ANY} s)
     (letE (s-exp->symbol (second (s-exp->list s))) 
           (parse-e (fourth (s-exp->list s))) 
           (parse-e (fourth (rest (rest (s-exp->list s))))))]
    [(s-exp-match? `{SYMBOL {ANY ...}} s)
     (appE (s-exp->symbol (first (s-exp->list s))) 
           (map parse-e (s-exp->list (second (s-exp->list s)))))]
    [else (error 'parse-e "syntax error")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))

; Ewaluator -------------------------------------------------------------------------------------------------------------------------------------

; Value 
(define-type-alias Value Number)

; Answer
(define-type Answer
  (numA [n : Number])
  (funV [b : (Answer -> Answer)]))

; Środowisko definicji (D)
(define-type-alias Def (Listof Definitions))

(define-type Definitions
  (attach [fname : Symbol]
          [body : D]))

(define mt-def empty)

(define (extend-def [def : Def] [x : Symbol] [e : D]) : Def
  (cons (attach x e) def))

(define (lookup-def [x : Symbol] [def : Def]) : D
  (type-case (Listof Definitions) def
    [empty (error 'lookup "unbound definition")]
    [(cons b rst-def) (cond
                        [(eq? x (attach-fname b))
                         (attach-body b)]
                        [else (lookup-def x rst-def)])]))

; Środowisko zmiennych
(define-type-alias Env (Listof Binding))

(define-type Binding
  (bind [name : Symbol]
        [ans : Answer]))

(define mt-env empty)

(define (extend-env [env : Env] [x : Symbol] [a : Answer]) : Env
  (cons (bind x a) env))

(define (lookup-env [n : Symbol] [env : Env]) : Answer
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? n (bind-name b))
                         (bind-ans b)]
                        [else (lookup-env n rst-env)])]))
; Operatory
(define (op->proc [op : Op]) : (Answer Answer -> Answer)
  (type-case Op op
    [(add) (λ (v1 v2) (numA (+ (numA-n v1) (numA-n v2))))]
    [(sub) (λ (v1 v2) (numA (- (numA-n v1) (numA-n v2))))]
    [(mul) (λ (v1 v2) (numA (* (numA-n v1) (numA-n v2))))]
    [(leq) (λ (v1 v2) (if (<= (numA-n v1) (numA-n v2)) (numA 0) (numA 1)))]))

; Ewaluator -------------------------------------------------------------------------------------------------------------------------------------
(define (eval-p [e : P] [env : Env] [def : Def]) : Answer
  (type-case P e
    [(defE d b) (eval-e b env (foldl (lambda (x acc) (extend-def acc (funE-x x) x)) def d))]))

(define (eval-d [e : D] [env : Env]  [def : Def]) : Answer
  (type-case D e
    [(funE x args b) (eval-funE args b env def)]))

(define (eval-e [e : E] [env : Env] [def : Def]) : Answer
  (type-case E e
    [(numE n) (numA n)]
    [(opE l op r) ((op->proc op) (eval-e l env def) (eval-e r env def))]
    [(ifzE e0 e1 e2) (if (= (numA-n (eval-e e0 env def)) 0) (eval-e e1 env def) (eval-e e2 env def))]
    [(varE x) (lookup-env x env)]
    [(appE b args)
     (let ([f (lookup-def b def)])
       (eval-appE (eval-d f env def) (map (lambda (x) (eval-e x env def)) args) env def))]
    [(letE x e1 e2)
     (let ([v (eval-e e1 env def)])
       (eval-e e2 (extend-env env x v) def))]))

; Eval-funE - tworzy kaskadę funkcji (funV) jedno argumentowych
(define (eval-funE [xs : (Listof Symbol)] [b : E] [env : Env] [def : Def]) : Answer
  (cond
    [(empty? xs) (error 'eval-funE "syntax error")]
    [(empty? (rest xs)) (funV (λ (v) (eval-e b (extend-env env (first xs) v) def)))]
    [else (funV (λ (v) (eval-funE (rest xs) b (extend-env env (first xs) v) def)))]))

; Apply - aplikuje argument do funkcji
(define (apply [f : Answer] [a : Answer]) : Answer
  ((funV-b f) a))

; Eval-appE - evaluuje funkcję argument, po argumencie
(define (eval-appE [f : Answer] [v2 : (Listof Answer)] [env : Env] [def : Def]) : Answer
  (cond
    [(empty? v2) (error 'eval-appE "type error")]
    [(empty? (rest v2)) (apply f (first v2))]
    [else (eval-appE (apply f (first v2)) (rest v2) env def)]))

; Translate - tłumaczy typ Answer na typ Value
(define (translate [a : Answer]) : Value
  (cond
    [(numA? a) (numA-n a)]
    [else (error 'translate "type error")]))

; Run -------------------------------------------------------------------------------------------------------------------------------------------
  
(define (run [s : S-Exp]) : Value
  (translate (eval-p (parse-p s) mt-env mt-def)))
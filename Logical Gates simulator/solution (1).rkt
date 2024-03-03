#lang racket
(require data/heap)
(provide sim? wire?
         (contract-out
          [make-sim        (-> sim?)]
          [sim-wait!       (-> sim? positive? void?)]
          [sim-time        (-> sim? real?)]
          [sim-add-action! (-> sim? positive? (-> any/c) void?)]
          [make-wire       (-> sim? wire?)]
          [wire-on-change! (-> wire? (-> any/c) void?)]
          [wire-value      (-> wire? boolean?)]
          [wire-set!       (-> wire? boolean? void?)]
          [bus-value (-> (listof wire?) natural?)]
          [bus-set!  (-> (listof wire?) natural? void?)]
          [gate-not  (-> wire? wire? void?)]
          [gate-and  (-> wire? wire? wire? void?)]
          [gate-nand (-> wire? wire? wire? void?)]
          [gate-or   (-> wire? wire? wire? void?)]
          [gate-nor  (-> wire? wire? wire? void?)]
          [gate-xor  (-> wire? wire? wire? void?)]
          [wire-not  (-> wire? wire?)]
          [wire-and  (-> wire? wire? wire?)]
          [wire-nand (-> wire? wire? wire?)]
          [wire-or   (-> wire? wire? wire?)]
          [wire-nor  (-> wire? wire? wire?)]
          [wire-xor  (-> wire? wire? wire?)]
          [flip-flop (-> wire? wire? wire? void?)]))

; Definicje ------------------------------------------------------------------------------------------

; Przewód
(struct wire
  ([sim]
   [signal #:mutable]
   [actions #:mutable]) #:transparent)

; Symulacja
(struct sim
  ([current-time #:mutable]
   [queue #:mutable]))

; Node od Heap'a
(struct node
  ([action-time #:mutable]
   [proc #:mutable]))

; Symulacja ------------------------------------------------------------------------------------------

; make-sim - tworzy symulacje
(define (make-sim)
  (sim 0 (make-heap (lambda (x y) (<= (node-action-time x) (node-action-time y))))))

; sim-wait! - przesuwa symulacje do przodu o podany czas, wykonując chronologicznie akcje z przeszłości
(define (sim-wait! sim timedx)
  (if (= (heap-count (sim-queue sim)) 0)
      (set-sim-current-time! sim (+ (sim-time sim) timedx))
      (let ([m (heap-min (sim-queue sim))]
            [temp (sim-time sim)])
        (if (< (+ (sim-current-time sim) timedx) (node-action-time m))
            (set-sim-current-time! sim (+ temp timedx))
            (begin (set-sim-current-time! sim (node-action-time m))
                   ((node-proc m))
                   (heap-remove-min! (sim-queue sim))
                   (sim-wait! sim (- (+ temp timedx) (sim-time sim))))))))

; sim-time - zwraca aktualny czas symulacji
(define (sim-time sim)
  (sim-current-time sim))

; sim-add-action! - dodaje procedure do heap'a
(define (sim-add-action! sim time proc)
  (heap-add! (sim-queue sim) (node (+ time (sim-time sim)) proc)))

; Przewody -------------------------------------------------------------------------------------------

; make-wire - tworzy przewód 
(define (make-wire s)
  (wire s #f '()))
    
; wire-value - zwraca wartość logiczną przewodu
(define (wire-value wire)
  (wire-signal wire))

; wire-set! - zmienia sygnał przewodu (jeżeli się zmienia to wywołuje wszystkie przypisane akcje)
(define (wire-set! wire new-signal)
  (if (equal? (wire-signal wire) new-signal)
      (void)
      (begin
        (set-wire-signal! wire new-signal)
        (for-each  (lambda (x) (x)) (wire-actions wire)))))

; wire-on-change! - dodaje do listy akcje która ma się "wykonać" przy każdym zmienienu wartości logicznej przewodu
(define (wire-on-change! wire proc)
  (begin (set-wire-actions! wire (cons proc (wire-actions wire)))
         (proc)))

; Bramki logiczne ------------------------------------------------------------------------------------

; (próbowałem dodać tutaj definicje gate-generic,
;  lecz przez time limity na webcat zostałem przy powtarzającej się implementacji bramek...)

; Gate-Not 
(define (gate-not output input)
  (let ([proc (lambda () (sim-add-action! (wire-sim output) 1 (lambda () (wire-set! output (not (wire-value input))))))])
    (wire-on-change! input proc)))

; Gate-And
(define (gate-and output input1 input2)
  (let ([proc (lambda () (sim-add-action! (wire-sim output) 1
                                          (lambda () (wire-set! output (and (wire-value input1) (wire-value input2))))))])
    (wire-on-change! input1 proc)
    (wire-on-change! input2 proc)))
; Gate-or
(define (gate-or output input1 input2)
  (let ([proc (lambda () (sim-add-action! (wire-sim output) 1
                                          (lambda () (wire-set! output (or (wire-value input1) (wire-value input2))))))])
    (wire-on-change! input1 proc)
    (wire-on-change! input2 proc)))

; Gate-nand
(define (gate-nand output input1 input2)
  (let ([proc (lambda () (sim-add-action! (wire-sim output) 1
                                          (lambda () (wire-set! output (nand (wire-value input1) (wire-value input2))))))])
    (wire-on-change! input1 proc)
    (wire-on-change! input2 proc)))

; Gate-nor
(define (gate-nor output input1 input2)
  (let ([proc (lambda () (sim-add-action! (wire-sim output) 1
                                          (lambda () (wire-set! output (nor (wire-value input1) (wire-value input2))))))])
    (wire-on-change! input1 proc)
    (wire-on-change! input2 proc)))

; gate-xor
(define (gate-xor output input1 input2)
  (let ([proc (lambda () (sim-add-action! (wire-sim output) 2
                                          (lambda () (wire-set! output (xor (wire-value input1) (wire-value input2))))))])
    (wire-on-change! input1 proc)
    (wire-on-change! input2 proc)))

; Przewody logiczne ----------------------------------------------------------------------------------

; wire-generic
(define (wire-generic input1 input2 gate)
  (let ([output (make-wire (wire-sim input1))])
    (gate output input1 input2)
    output))

; wire-not
(define (wire-not input)
  (let ([output (make-wire (wire-sim input))])
    (gate-not output input)
    output))

; wire-and
(define (wire-and input1 input2)
  (wire-generic input1 input2 gate-and))

; wire-or
(define (wire-or input1 input2)
  (wire-generic input1 input2 gate-or))

; wire-nor
(define (wire-nor input1 input2)
  (wire-generic input1 input2 gate-nor))

; wire-xor
(define (wire-xor input1 input2)
  (wire-generic input1 input2 gate-xor))

; wate-nand
(define (wire-nand input1 input2)
  (wire-generic input1 input2 gate-nand))

; bus & flip-flop ------------------------------------------------------------------------------------

; bus-set!
(define (bus-set! wires value)
  (match wires
    ['() (void)]
    [(cons w wires)
     (begin
       (wire-set! w (= (modulo value 2) 1))
       (bus-set! wires (quotient value 2)))]))

; bus-value
(define (bus-value ws)
  (foldr (lambda (w value) (+ (if (wire-value w) 1 0) (* 2 value)))
         0
         ws))

; flip-flop
(define (flip-flop out clk data)
  (define sim (wire-sim data))
  (define w1  (make-wire sim))
  (define w2  (make-wire sim))
  (define w3  (wire-nand (wire-and w1 clk) w2))
  (gate-nand w1 clk (wire-nand w2 w1))
  (gate-nand w2 w3 data)
  (gate-nand out w1 (wire-nand out w3)))
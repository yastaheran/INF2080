(load "huffman.scm")

;;Oppgave 1A
;(define (p-cons x y)
 ; (lambda (proc) (proc x y)))

;(define (p-car param)
;  (param (lambda arg1 arg2) arg1))

;(define (p-cdr param)
;  (param (lambda arg1 arg2) arg2))

;;Oppgave 1B
;(define foo 42)     ;;   - setter foo til 42

;;(let ((foo 5)          - evalueres til different
;;       (x foo))
;;   (if (= x foo)
;;       'same
;;       'different))
;((lambda (foo x) (if(= x foo) 'same 'different)) 5 foo) 

;; (let ((bar foo)        - evalueres til (towel (42 towel))
;;       (baz 'towel))
;;   (let ((bar (list bar baz))
;;         (foo baz))
;;      (list foo bar)))
;((lambda (bar baz) (lambda (bar foo) (list foo bar) (list bar baz) baz)) foo 'towel)

;;Oppgave 1C
;;(define infix-eval)

; Kalleksempler
;;(define foo (list 21 + 21))
;;(define baz (list 21 list 21))
;;(define bar (list 84 / 2))
;;;(infix-eval foo)
;(infix-eval baz)
;(infix-eval bar)

;;Oppgave 1D
;;
;;(define bah '(84 / 2))
;;(infix-eval bah)

;;Oppgave 2A
;(define (member? pred symbol items)
 ; (if (null? pred) #f))) ;;sjekke om pred
  ;;(cond ((eq? null? items) #f)
    ;;    ((eq? symbol (car items) #t)
      ;;   (else (member? pred symbol (cdr items)))))"

;;Oppgave 2B
;(define (decode-hale bit tree)
 ;(define (decode-iter bits current-branch)
  ; (if(null? bits)
   ;   message
    ;  (let((next-branch
     ;       (choose-branch (car bits) current-branch)))
      ;  (if(leaf? next-branch)
       ;    (decode-iter (append message (cons (symbol-leaf next-branch) '()))
        ;                (cdr bits)
         ;               tree)
          ; (decode-iter message (cdr bits) next-branch)))))
  ;(decode-iter '() bit tree))

;;Oppgave 2C
;;resultatet: (ninjas fight ninjas by night)

;;Oppgave 2D
(define (encode message tree)
  (define (encode-1 symbol current-branch) ;tar en symbol om gangen
    (if(leaf? current-branch)
       '()
       (let ((left (left-branch current-branch))
             (right (right-branch current-banch)))
         (cond ((member? = symbol (symbols left)) (cons 0 (encode-1 symbol left)))
               ((member? = symbol (symbol right)) (cons 1 (encode-1 symbol right)))
               (else '()))))
    (if(null? message)
       '()
       (append (encode-1 (car message) tree)
               (encode (cdr message) tree)))))

;;Oppgave 2E
;(define (grow-huffman-tree freqs)
 ; (define (ght-iter set) ;antar at 'set' ikke er tom
  ;  (if(null? (cdr set))
   ;    (car set)         ;ferdig når man når rota
    ;   (ght-iter (adjoin-set (make-code-tree (cadr set) (car set))
     ;                        (cddr set)))))
  ;(ght-iter (make-leaf-set freqs)))

;;Oppgave 2F
;(define codebook(grow-huffman-tree
 ;                '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3) (in 3)
  ;                 (ambush 2) (defeat 1) (the 5) (sword 4) (by 12) (assassin 1) (river 2)
   ;                (forest 1) (wait 1) (poison 1))))

;(encode '(ninjas fight ninjas fight ninjas ninjas fight samurais samurais fight samurais
 ;                fight ninjas ninjas fight by night)
  ;      codebook)
; Resultatet blir (1010110101101101010000010001101101011000111). Det er 43 bits til sammen, og
; 43/17 = 2.5 bits pr ord. Minste antall bits for en kode med fast lengde vil være 4 bits pr 
; ord (alfabetet har 16 ord og vi trenger dermed 2^4 forskjellige bitmønstre).

;;Oppgave 2G
;(define (huffman-leaves tree)
 ; (if (leaf? tree)
  ;    (cons (cdr tree) '())
   ;   (append (huffman-leaves (left-branch tree))
    ;          (huffman-leaves (right-branch tree)))))

;;Oppgave 2H
(define (expected-code-length)
  (let ((total-weight (cadddr tree)))
    (define (traverse branch)
      (if (leaf? branch)
          (* (/ (caddr branch) total-weight)    ;;3.elementet i branchen delt på totalvekt
             (length (encode (cons (cadr branch) '()) tree))) ;; ganget med antall bits
          (+ (traverse (left-branch branch))
             (travers (right-branch branch))))
      (traverse tree))))
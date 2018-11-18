;; Oblig 3b - kjervi, ingerng, yast

(load "evaluator.scm")
(set! the-global-environment (setup-environment))
; Klart for å evaluere uttrykk i den nye Scheme-implementasjonen vår. F.eks:
;(mc-eval '(+ 1 2) the-global-environment)
(read-eval-print-loop)
;(+ 1 2)

;; INGER - fjenr denne linjen før levering

;; Oppgave 1a
(newline)(display "Tester for oppgave 1a")
;; Oppgave 2a
(newline)(display "Tester for oppgave 2a")

;; Oppgave 2b
(newline)(display "Tester for oppgave 2b")


;; Endret fra evaluator.scm:
; eval-special-form:
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (ny-and exp env)) ;; 3a
        ((or? exp) (ny-or exp env))))   ;; 3a

; special-form?
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; 3a
        ((or? exp) #t)  ;; 3a 
        (else #f)))

;; Oppgave 3a
;; Legger til or og and som special forms i evaluatoren med de nødvendige syntsaks-prosedyrer
;; og evalueringsregler som dette krever. OK om and/or returnerer boolske konstanter

; and:
(define (and? exp) (tagged-list? exp 'and))
(define (ny-and exp env)
  (define (leit args)
    (let ((value (mc-eval (car args) env)))
      (cond ((false? value) #f)
            ((null? (cdr args)) value)
            (else (leit (cdr args))))))

  (if (null? (cdr exp)) #t
      (leit (cdr exp))))


; or:
(define (or? exp) (tagged-list? exp 'or))

(define (ny-or exp env)
  (define (leit args)
    (let ((value (mc-eval (car args) env)))
      (cond ((false? value) #f)
            ((null? (cdr args)) value)
            (else (leit (cdr args))))))
  (if (null? (cdr exp)) #t
      (leit (cdr exp))))
 


;; Tester oppgave 3b med diverse eksempelr
(newline)(display "Tester for oppgave 3a") (newline)
(mc-eval '(and 'true 'true 'true) the-global-environment) ;; true
(mc-eval '(and 'yes 'and 'no) the-global-environment) ;; no
(mc-eval '(or 'false 'true 'false) the-global-environment) ;; #f
(mc-eval '(or 'no 'true 'yes) the-global-environment) ;; yes
(mc-eval '(and (= 3 6) (= 1 4)) the-global-environment) ;#f
(mc-eval '(or (= 2 4) (= 2 2)) the-global-environment) ;#f
(mc-eval '(and (= 5 5) (= 5 5)) the-global-environment) ; #t

;; Oppgave 3b
;; IMplementer en ny syntaks for if i evaluatoren som lar oss skrive uttrykk på
;; dnene formen:
; (if <test1>
; then <utfall1>
; then <utfall2>
; else <utfall3>)
;; Et uttrykk kan ha vilkårlig mange elsif-grener (inkl. 0) og else er obligatorisk
(define (if-ny exp) (tagged-list? (cddr exp) 'then))

(define (ny-if exp env)
  (let ((value (mc-eval (if-predicate exp) env)))
    (cond ((tagged-list? exp 'else) value)
          ((true? value) (mc-eval (cadddr exp) env))
          (else (ny-if (cddddr exp) env)))))


; Endrer i prekoden:
(define (eval-if exp env)
  (if (if-ny exp) ;; Utvidet for 3b
      (ny-if exp env) ;; Utvider for 3b
      (if (true? (mc-eval (if-predicate exp) env))
          (mc-eval (if-consequent exp) env)
          (mc-eval (if-alternative exp) env))))

;; Tester oppgave 3b med diverse eksempelr
(newline)(display "Tester for oppgave 3b")(newline)
(mc-eval '(if 'false 'yes #f) the-global-environment) ; #f
(mc-eval '(if #t then 'yes elsif #f then 'no else undefined) the-global-environment) ;; yes
(mc-eval '(if #t then 'yes elsif #t then 'yes else 'no) the-global-environment) ;; yes
(mc-eval '(if (= 9 (+ 3 6)) then #t else #f)the-global-environment) ; #t
(mc-eval '(if (= 2 3) then #t elsif (= 3 3) then '3=3 else #f) the-global-environment) ; 3=3


;; Oppgave 3c
;; Støtte for let
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; 3a
        ((or? exp) #t)  ;; 3a
        ((let? exp) #t) ;; 3c/d
        (else #f)))     ;; 3c/d

(define (let? exp) (tagged-list? exp 'let))

;;endringer i eval-special-form
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (ny-and exp env)) ;; 3a
        ((or? exp) (ny-or exp env))   ;; 3a
        ((let? exp) (mc-eval (let3c? exp) env)))) ;;3c/d

(define (let3c? exp) (list? (cadr exp)))

(define (eval-let exp env)
  (if (let3c? exp)
      (mc-eval (let3c->lambda exp) env)
      (mc-eval (let3d->lambda exp) env)))

(define (make-lambda-exp var body args)
  (cons (cons 'lambda (cons var body) args)))

(define (let3c->lambda exp)
  (let ((vars (map car (cadr exp)))
        (body (cddr exp))
        (args (map cadr (cadr exp))))
    (build-lambda-exp vars body args)))

;; Oppgave 3d
;; (define (let3d->lambda exp))

(display "Tester for oppgave 3c og 3d")
(mc-eval '(let ((x 2) (y 3)) (display "2 + 3 = ") (+ x y))
         the-global-environment) ;#t
(mc-eval '(let x = 4 and y = 3 in (display "4 - 3 = ") (- x y))
         the-global-environment) ;#f

;; Oppgave 3e
; Hvert while-uttrykk i vår implementasjon har 3 deler:
; 1. Et predikat
; 2. En liste av instruksjoner som vil bli utført hvis predikatet gir sann
; 3. En liste av oppdatering-operasjoner

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;;3a
        ((or? exp) #t)  ;;3a
        ((let? exp) #t) ;;3c/d
        ((while? exp) #t) ;;3e
        (else #f)))

(define (while? exp) (tagged-list? exp 'while))

;;endringer i eval-special-form
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (ny-and exp env)) ;; 3a
        ((or? exp) (ny-or exp env))   ;; 3a
        ((let? exp) (mc-eval (let3c? exp) env)) ;;3c/d
        ((while? exp) (mc-eval(eval-while exp env) env)))) ;;3e

(define (eval-while exp env)
  (if (true? (mc-eval (cadr exp) env))
      (begin (mc-eval (sequence->exp (caddr exp)) env) ;body
      (mc-eval (sequence->exp (caddr exp)) envt)
      (eval-while exp env))))

(display "Test for oppgave 3e")
(mc-eval '(define x 0) the-global-environment) ;OK
(mc-eval '(define y 9) the-global-environment) ;OK
(define loop '(while (and (< x 10) (>= y 7))
                     ((display x) (display " ") (display y) (display " _ "))
                     ((set! x (+ x 1)) (set! y (- y 1)))))
(mc-eval loop the-global-environment)
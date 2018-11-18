;; Oblig 3b - kjervi, ingerng, yast

(load "evaluator.scm")
(set! the-global-environment (setup-environment))
(read-eval-print-loop)


;; Oppgave 1a
(newline)(display "Tester for oppgave 1a")
;; (a)
;; Evaluerer følgende uttrykk i REPL`en i den metasirkluære evaluatoren:
;; Definerer først metoder i MC-evaluator REPL`en.

;; (define (foo cond else)
;;   (cond ((= cond 2) 0)
;;         (else (else cond))))
;; --> MC-Eval value:   ok

;; (define cond 3)
;; --> MC-Eval value:   ok

;; (define (else x) (/ x 2))
;; --> MC-Eval value:   ok

;; (define (square x) (* x x))
;; --> MC-Eval value:   ok

;; Evaluerer følgende uttrykk i MC-Evaluatoren.
;; Svarer også på følgende spørsmål fra oppgaveteksten:
;; 1) "Hva blir returnert?"
;; 2) "Hvorfor får vi disse returverdiene med følgende uttrykk?"

;; (foo 2 square)
;; --> MC-Eval value:   0

;; (foo 2 square) blir evaluert til 0.
;; Grunnen til dette 

;; (foo 4 square)
;; --> MC-Eval value:   16
;; (foo 4 square) blir evaluert til 16.
;; Grunnen til dette


;; (cond ((= cond 2) 0)
;;       (else (else 4)))
;; --> MC-Eval value:   2
;; (cond ((= cond 2) 0)
;;       (else (else 4))) blir evaluert til 2.
;; Grunnen til dette

;; Oppgave 2a
(newline)(display "Tester for oppgave 2a")
;; Legger til prosedyrene 1+ og 1- som innebygde primtiver i evaluatoren.
;; Prosedyrene skal ta ett argument og returnere verdien av å henholdsvis legge til eller trekke fra 1.

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
;; Legger til i prosedyren for å kunne utføre 1+ og 1-.
        (list '1+ (lambda (x) (+ x 1)))
        (list '1- (lambda (x) (- x 1)))))
;; Tester at det som er lagt til fungerer.
(mc-eval '(1+ 2) the-global-environment)
(mc-eval '(1- 2) the-global-environment)

;; Oppgave 2b
(newline)(display "Tester for oppgave 2b")

(define (install-primitive! proc expr)
  (set-car! (car the-global-environment)
        (append (caar the-global-environment) (list proc)))
  (set-cdr! (car the-global-environment)
        (append (cdar the-global-environment) (list (list 'primitive expr)))))

;;test av install-primitive
(mc-eval (install-primitive! 'square (lambda (x) (* x x))) the-global-environment)


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
        ((and? exp) (ny-and exp env)) ;; for 3a
        ((or? exp) (ny-or exp env)))) ;; for 3a
 
; special-form?
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; For 3a
        ((or? exp) #t) ;; For 3a
        (else #f)))


; eval-if
(define (eval-if exp env)
  (if (if-ny exp) ;;  for 3b
      (ny-if exp env) ;; for 3b
      (if (true? (mc-eval (if-predicate exp) env))
          (mc-eval (if-consequent exp) env)
          (mc-eval (if-alternative exp) env))))



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
 


;; Tester oppgave 3a med diverse eksempler
(newline)
(newline)(display "Tester for oppgave 3a") (newline)
(mc-eval '(or (= 2 2) (= 2 2)) the-global-environment) ; #t
(mc-eval '(or false false false) the-global-environment) ; #f
(mc-eval '(and (= 5 5) (= 5 5)) the-global-environment) ; #t
(mc-eval '(and (= 3 6) (= 2 1)) the-global-environment) ; #f

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
  (define (ny-next exp) (cddddr exp))
  (define (ny-else exp) (cadr exp))
  (define (ny-then exp) (cadddr exp))
  (define (else? exp) (tagged-list? exp 'else))
  (cond ((else? exp) (mc-eval (ny-else exp) env))
        ((true? (mc-eval (if-predicate exp) env))
         (mc-eval (ny-then exp) env))
        (else (ny-if (ny-next exp) env))))


;; Tester oppgave 3b med diverse eksempler
(newline)(display "Tester for oppgave 3b")(newline)
(mc-eval '(if #t then 'yes elsif #t then 'yes else 'no) the-global-environment) ;; yes
(mc-eval '(if (= 9 (+ 3 6)) then #t else #f)the-global-environment) ; #t
(mc-eval '(if (= 2 3) then #t elsif (= 3 3) then '3=3 else #f) the-global-environment) ; 3=3
(newline)

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
        ((and? exp) #t) 
        ((or? exp) #t)  
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
        ((and? exp) (ny-and exp env)) 
        ((or? exp) (ny-or exp env))   
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
        ((and? exp) #t) 
        ((or? exp) #t)  
        ((let? exp) #t) 
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
        ((and? exp) (ny-and exp env)) 
        ((or? exp) (ny-or exp env))   
        ((let? exp) (mc-eval (let3c? exp) env))
        ((while? exp) (mc-eval(eval-while exp env) env)))) ;;3e

(define (eval-while exp env)
  (if (true? (mc-eval (cadr exp) env))
      (begin (mc-eval (sequence->exp (caddr exp)) env) ;body
      (mc-eval (sequence->exp (caddr exp)) env)
      (eval-while exp env))))

;;legger til nye primitiver
(install-primitive! '< <) 
(install-primitive! '> >)
(install-primitive! '<= <=)
(install-primitive! '>= >=)

(display "Test for oppgave 3e")
(mc-eval '(define x 7) the-global-environment) ;OK
(mc-eval '(define y 9) the-global-environment) ;OK
;evig løkke....
;(define loop '(while (and (< x 10) (>= y 7))
  ;                   ((display x) (display " ") (display y) (display " _ "))
 ;                    ((set! x (+ x 1)) (set! y (- y 1)))))
;(mc-eval loop the-global-environment)

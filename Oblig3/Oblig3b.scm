(load "evaluator.scm")

;;3c
(define (let? exp) (tagged-list? exp 'let))

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

;;3d
;;(define (let3d->lambda exp))

;;Test for 3c og 3d
(mc-eval '(let ((x 2) (y 3)) (display "2 + 3 = ") (+ x y))
         the-global-environment)
(mc-eval '(let x = 4 and y = 3 in (display "4 - 3 = ") (- x y))
         the-global-environment)

;;3E
; Hvert while-uttrykk i vår implementasjon har 3 deler:
; 1. Et predikat
; 2. En liste av instruksjoner som vil bli utført hvis predikatet gir sann
; 3. En liste av oppdatering-operasjoner

(define (while? exp) (tagged-list? exp 'while))

(define (eval-while exp env)
  (if (true? (mc-eval (cadr exp) env))
      (begin (mc-eval (sequence->exp (caddr exp)) env) ;body
      (mc-eval (sequence->exp (caddr exp)) envt)
      (eval-while exp env))))

;;Test for 3E
(mc-eval '(define x 0) the-global-environment)
(mc-eval '(define y 9) the-global-environment)
(define loop '(while (and (< x 10) (>= y 7))
                     ((display x) (display " ") (display y) (display " _ "))
                     ((set! x (+ x 1)) (set! y (- y 1)))))
(mc-eval loop the-global-environment) ;0 9 _ 1 8 _ 2 7
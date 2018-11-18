(load "prekode3a.scm")

;;1a og b
(define proc-table (make-table))              ;;tabell for de memoriserte prosedyrene

(define (memoize proc)
  (let ((table (make-table)))
    (lambda args (or (lookup args table)
                     (let ((result (apply proc args)))
                       (begin (insert! args result table) result))))))

(define (unmemoize proc)
 (or (lookup proc proc-table) proc))

(define (mem msg proc)
  (cond ((eq? msg 'memoize)
         (let ((new-proc (memoize proc)))
           (begin (insert! new-proc proc proc-table) new-proc)))
        ((eq? msg 'unmemoize)
         (unmemoize proc))
         (else (display "ukjent kommando"))))

;;1c
; Når vi bruker prosedyren mem-fib, så er det bare det første rekursive kallet som blir
; utført som den nye memoiserte versjonen av fib, og resten av kallene blir utført som den
; originale versjonen i prekoden. Dette her leder til at det bare er det første resultatet
; som blir lagret i cachen, men ikke de andre resultatene, og prosedyren lookup (som søker
; etter elementene i cachen) utføres bare i det første rekursive kallet.
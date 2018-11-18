;; 3C
(define (member? obj lst)
  (cond ((null? lst) #f)                   
        ((eq? obj (car lst)) #t)
        (else (member? obj (cdr lst)))))

(define (cycle? lst)
  (define (iter ikke-besokt besokt)
    (cond ((null? ikke-besokt) #f)
          ((member? ikke-besokt besokt) #t)
          (else (iter (cdr ikke-besokt)
                      (cons ikke-besokt besokt)))))
  (iter lst '()))

;;test
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))
(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
(set-car! (car bah) 42)

(newline) (display "Test for 3c") (newline)
(cycle? '(hey ho))   
(cycle? '(la la la)) 
(cycle? bah)         
(cycle? bar)         

;; 3D
;; I Scheme er en liste definert som et kjede av cons-par der siste elementet er '(), den
;; den tomme listen. Prosedyren list? sjekker dermed om cdr-en til siste cons-cellen faktisk
;; peker til '() eller ikke. Men siden det ikke er en "siste" cons-celle i en sirkulær liste
;; (elementene i listen gjentas i en uendelig løkke), så vil list? returnere false hvis
;; input-argumentet er en sirkulær liste (som f.eks. bar)

;; 3E
(define (copy-lst lst)
  (append lst '()))

(define (top ring)
  (ring 'top))

(define (left-rotate! ring)
  (ring 'left-rotate!))

(define (right-rotate! ring)
  (ring 'right-rotate!))

(define (insert! ring item)
  (ring 'insert! item))

(define (delete! ring)
  (ring 'delete!))

(define (make-ring input)
  (define ring (copy-lst input))
  (define (make-ring! p)          ;ring peker
    (if (null? (cdr p))
       (set-cdr! p ring)          ;peker på første cons-celle
       (make-ring! (cdr p))))
  (define (top)
    (if (null? ring)
       '()
       (car ring)))
  (define (left-rotate!)
    (if (not (null? ring))
        (set! ring (cdr ring)))
    (top))
  (define (right-rotate!)
    (define (loop peker)
      (if (eq? (cdr peker) ring)
          (set! ring peker)
          (loop (cdr peker))))
    (if (not (null? ring))
        (loop ring))
    (top))
  (define (insert! item)
    (cond ((null? ring) (set! ring (list item))
                        (set-cdr! ring ring)
                        (top))
          (else (right-rotate!)
                (set-cdr! ring (cons item (cdr ring)))
                (left-rotate!))))
  (define (delete!)
    (cond ((or (null? ring) (eq? (cdr ring) ring)) (set! ring '()) (top))
          (else (right-rotate!)
                (set-cdr! ring (cddr ring))
                (left-rotate!))))
  (define (dispatch msg . item)
    (cond ((eq? msg 'top) (top))
          ((eq? msg 'left-rotate!) (left-rotate!))
          ((eq? msg 'right-rotate!) (right-rotate!))
          ((eq? msg 'insert!) (insert! (car item)))
          ((eq? msg 'delete!) (delete!))))
  (if (not (null? input))
      (make-ring! ring))
  dispatch)

;;test
(define r1 (make-ring '(1 2 3 4)))
(define r2 (make-ring '(a b c d)))
(top r1)
(top r2)
(right-rotate! r1)
(left-rotate! r1)
(left-rotate! r1)
(delete! r1)
(left-rotate! r1)
(left-rotate! r1)
(left-rotate! r1)
(insert! r2 'x)
(right-rotate! r2)
(left-rotate! r2)
(left-rotate! r2)
(top r1)
;;Oppgave 1A
;;(cons 47 11)
;;       ___________
;; 	|     |     |
;; 	|  o  |  o--|----> 11
;; 	|__|__|_____|
;; 	   |
;; 	   |
;; 	   47

;;Oppgave 1B
;;(cons 47 '())
;;       _________
;;	|     |  /|
;; 	|  o  | / |
;; 	|__|__|/__|
;; 	   |
;; 	   |
;; 	   47

;;Oppgave 1C
;;(list 47 11)
;; 	 ___________       _________
;; 	|     |     |     |     |  /|
;; 	|  o  |  o--|---->|  o  | / |
;; 	|__|__|_____|     |__|__|/__|
;; 	   |                 |
;; 	   |                 |
;; 	   47                11

;;Oppgave 1D
;;'(47 (11 12))
;; 	 ___________       _________
;; 	|     |     |     |     |  /|
;; 	|  o  |  o--|---->|  o  | / |
;; 	|__|__|_____|     |__|__|/__|
;; 	   |                 |
;; 	   |               __|________       _________ 
;; 	   47             |     |     |     |     |  /|
;; 	                  |  o  |  o--|---->|  o  | / |
;; 	                  |__|__|_____|     |__|__|/__|
;; 	                     |                 |       
;; 	                     |                 |
;; 	                     11                12
;;
;;Oppgave 1E
;;(define foo '(1 2 3))
;;(cons foo foo) --> ((1 2 3) 1 2 3)
;;         +-------+-------+
;;         |   o   |   o   |
;;         |   |   |   |   |
;;         +---+---+---+---+
;;             |     /
;;             |    /
;;             |   / 
;;            +-------+-------+        +-------+-------+        +-------+-------+
;;    foo---> |   o   |   o---+------> |   o   |   o---+------> |   o   |   /   |
;;            |   |   |       |        |   |   |       |        |   |   |  /    |
;;            +---+---+-------+        +---+---+-------+        +---+---+-------+
;;                |                        |                        |
;;                |                        |                        |
;;                2                        2                        3

;;Oppgave 1F
(car (cdr (cdr '(0 42 #t bar))))

;;Oppgave 1G
(car (car (cdr '((0 42) (#t bar)))))

;;Oppgave 1H
(car (car (cdr (cdr '((0) (42) (#t) (bar))))))

;;Oppgave 1I 
(list(list 0 42)(list #t bar))

(cons(cons 0(cons 42 '()))
     (cons (cons #t(cons bar '()))'()))

;;Oppgave 2A
(define (length2 items)
  (define (len-iter lst teller)
    (if (null? lst)
        teller
        (len-iter (cdr lst) (+ 1 teller))))
  (len-iter items 0))

(length2 '(1 2 3 4 6))

;;Oppgave 2B
;;Denne metoden bruker halerekursjon. 
(define (reduce-reverse items)
    (define (reduce-reveres-iter a b)
      (if(null? a)
       b
       (reduce-reverse-iter(cdr a) (cons(car a)b))))
  (reduce-reverse-iter items '()))

;;Oppgave 2C
(define (all? pred lst)
  (cond((null? lst)#t)
       ((pred (car lst))
        (if(car lst)
           (all? pred (cdr lst))))
        (else #f)))

(all? (lambda (x y) (and(< y 10)))
      '(1 2 3 4 5))

(all? (lambda (x y) (and(< y 10)))
      '(1 2 3 4 50))

;;Oppgave 2D
(define (nth pos liste)
  (define (iter liste teller)
    (if (null? liste)
        #f
    (if (equal? pos teller)
        (car liste)
        (iter (cdr liste)
              (+ 1 teller)))))
  (iter liste 0))
(nth 2 '(47 11 12 13))

;;Oppgave 2E
(define (where pos liste)
  (define (iter liste teller)
    (if (null? liste)
        #f
    (if (equal? (car liste) pos)
        teller
        (iter (cdr liste)
              (+ 1 teller)))))
  (iter liste 0))

(where 3 '(1 2 3 3 4 5 3))

;;Oppgave 2F
(define (map2 proc liste1 liste2)
  (cond ((null? liste1) '())
        ((null? liste2) '())
    (else
     (cons (proc (car liste1) (car liste2))
           (map2 proc (cdr liste1) (cdr liste2))))))
(map2 + '(1 2 3 4) '(3 4 5))

;;Oppgave 2G
;;gjennomsnittet av to tall
(map2 (lambda (a b) (/ (+ a b) 2))
      '(1 2 3 4) '(3 4 5))

;;sjekke om begge er partall
(map2 (lambda (a b) (and (even? a) (even? b)))
      '(1 2 3 4) '(3 4 5))

;;Oppgave 2H
(define (both? pred)
  (lambda (x y)
    (and (pred x)
       (pred y))))
((both? even?) 2 4)

;;Oppgave 2I
(define (self proc) (lambda (x) (proc x x)))

((self +) 5)
((self *) 3)
(self +)
((self list) "hello")
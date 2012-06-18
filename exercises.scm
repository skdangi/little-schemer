#lang racket
(define atom?
   (lambda (a)
      (not (list? a))))

;determine if the list contains only atoms
(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l))(lat? (cdr l)))
          (else #f))))

;detemine if an atom is a member of lat
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

;remove an atom from lat
(define rember
  (lambda (a lat)
    (cond ((null? lat) lat)
          ((eq? a (car lat)) (cdr lat))
          (else (cons (car lat) (rember a (cdr lat)))))))

;argmunt list - containing non empty lists; make a new list containing the first s expressions of the of containing lists of the argument list
(define firsts
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (car (car l)) (firsts (cdr l)))))))

;remove all the occurances of an atom from a list
(define rember*
  (lambda (a l)
    (cond ((null? l) l)
          ((eq? a (car l)) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l)))))))

;insert new atom in the left position of first old atom in a lat
(define insertR
  (lambda (new old lat)
    (cond ((null? lat) lat)
          ((eq? old (car lat)) (cons new lat))
          (else (cons (car lat) (insertR new old (cdr lat)))))))


;insert new atom in the left position of all old atom in a lat
(define insertR+
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) 
           (cons  new (cons (car lat) (insertR+ new old (cdr lat)))))
          (else (cons (car lat) (insertR+ new old (cdr lat)))))))

;insert new atom in the left position of all old atom in a list
(define insertR*
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((atom? (car lat))
                (cond ((eq? old (car lat)) 
                       (cons  new (cons (car lat) (insertR* new old (cdr lat)))))
                      (else 
                       (cons (car lat) (insertR* new old (cdr lat))))))
          (else 
           (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))

; count the no of occurances of an atom in a list
(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((atom? (car l))
                (cond ((eq? a (car l)) 
                       (+ 1 (occur* a (cdr l))))
                      (else 
                       (occur* a (cdr l)))))
          (else 
           (+ (occur* a (car l)) (occur* a (cdr l)))))))
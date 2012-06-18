#lang racket
(define atom?
   (lambda (a)
      (not (list? a))))
(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l))(lat? (cdr l)))
          (else #f))))
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
(define rember
  (lambda (a lat)
    (cond ((null? lat) lat)
          ((eq? a (car lat)) (cdr lat))
          (else (cons (car lat) (rember a (cdr lat)))))))
(define firsts
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (car (car l)) (firsts (cdr l)))))))
(define rember*
  (lambda (a lat)
    (cond ((null? lat) lat)
          ((eq? a (car lat)) (rember* a (cdr lat)))
          (else (cons (car lat) (rember* a (cdr lat)))))))
(define insertR
  (lambda (new old lat)
    (cond ((null? lat) lat)
          ((eq? old (car lat)) (cons new lat))
          (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertR+
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) 
           (cons  new (cons (car lat) (insertR+ new old (cdr lat)))))
          (else (cons (car lat) (insertR+ new old (cdr lat)))))))

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
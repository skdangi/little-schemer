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

;substitutes the value of old atom with new atom in the list
(define subst*
  (lambda (new old  l)
    (cond ((null? l) '())
          ((atom? (car l))
                (cond ((eq? old (car l)) 
                       (cons new (subst* new old (cdr l))))
                      (else 
                       (cons (car l) (subst* new old (cdr l))))))
          (else 
           (cons (subst* new old  (car l)) (subst* new old (cdr l)))))))

;compares if two atoms are equal
(define eqan
  (lambda (a1 a2)
    (cond ((and (number? a1)(number? a2))(= a1 a2))
          ((and (atom? a1)(atom? a2))(eq? a1 a2))
          (else #f))))


;compares if two lists containing s-expressions are equal
(define eqlist?
  (lambda (l1 l2)
    (cond ((null? l1) (null? l2))
          ((atom? (car l1))
                (cond ((atom? (car l2))
                       (and (eqan (car l1) (car l2))(eqlist? (cdr l1)(cdr l2)))) 
                      (else #f)))
          (else 
           (cond ((or (null? l2) (atom? (car l2))) #f)                   
                 (else 
                  (and (eqlist? (car l1)(car l2))(eqlist? (cdr l1) (cdr l2)))))))))

;compares if two s-expressions are equal
(define equal?
  (lambda (s1 s2)
    (cond
      ((null? s1)(null? s2))
      ((atom? s1)
       (cond 
          ((atom? s2)(eqan s1 s2))
          (else #f)))
      (else
       (cond ((atom? s2) #f)
             (else (eqlist? s1 s2)))))))

; remove an s-expression from the list
(define rembers
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? s (car l)) (rembers s (cdr l)))
      (else (cons (car l) (rembers s (cdr l)))))))

;check if representation of arithmetic expression contains numbers only
(define numbered?
  (lambda (ae)
    (cond ((null? ae) #t)
          ((atom? (car ae))
           (cond
             ((null? (cdr ae))(number? (car ae)))
             ((number? (car ae))(numbered? (cdr (cdr ae))))
             (else #f)))
          (else
           (cond 
             ((null? (cdr ae))(numbered? (car ae)))
             (else               
              (and (numbered? (car ae))(numbered? (cdr (cdr ae))))))))))

; checks whether lat is a set or not                   
(define set?
  (lambda (lat)
    (cond ((null? lat) #t)
          ((member? (car lat) (cdr lat)) #f)
          (else (set? (cdr lat))))))

; makeset using member?
(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
          ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
          (else (cons (car lat)(makeset (cdr lat)))))))


; makeset using remove*
(define makesetr*
  (lambda (lat)
    (cond ((null? lat) '())
          (else 
           (cons (car lat) 
                  (makesetr*       
                   (rember* (car lat) (cdr lat))))))))
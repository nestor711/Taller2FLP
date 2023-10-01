#lang eopl
<FNC>      := ('fnc <int> <and-list>)

<and-list> := (<and-exp> <and-list>)
           := (<and-exp>)

<or-list>  := (<or-exp> <or-list>)
           := (<or-exp>)

<and-exp>  := ('and <or-exp> <or-exp>)
<or-exp>   := ('or <variable> <variable>)
<variable> := <int-positivo>

; Constructores
(define (fnc n and-list)
  (list 'fnc n and-list))

(define (and-list exps)
  (cons 'and-list exps))

(define (or-list exps)
  (cons 'or-list exps))

(define (and-exp left right)
  (list 'and left right))

(define (or-exp left right)
  (list 'or left right))

(define (variable value)
  value)

; Extractores
(define (fnc->var fnc)
  (cadr fnc))

(define (fnc->clausulas fnc)
  (caddr fnc))

(define (or->varlist or-exp)
  (cdr or-exp))

(define (and->andlist and-exp)
  (cdr and-exp))

; Crear instancias SAT
(define sat1 (fnc 3 (and-list (and-exp (or-exp (variable 1) (variable 2))
                                      (or-exp (variable 3) (variable 4))))))

(define sat2 (fnc 2 (and-list (or-exp (variable 1) (variable 2))
                              (or-exp (variable 3) (variable 4))))))

(define sat3 (fnc 4 (and-list (or-exp (variable 1) (variable 2))
                              (and-exp (or-exp (variable 3) (variable 4))
                                       (and-exp (variable 5) (variable 6))))))

# Gramatica para los Datatypes
<FNC>      := ('fnc' <int> <and-list>)

<and-list> := (<and-exp> <and-list>)
           := (<and-exp>)
           := ε

<or-list>  := (<or-exp> <or-list>)
           := (<or-exp>)
           := ε

<and-exp>  := ('and' <or-exp> <or-exp>)

<or-exp>   := ('or' <variable> <variable>)

<variable> := <int-positivo>
:= ε para indicar que una lista puede ser vacía

(define-datatype expression expression?
  (fnc-exp
   (num-variable number?)
   (and-list and-list?))
  (and-list
   (and-exp and-exp?)
   (rest-and and-list?))
  (or-list
   (or-exp or-exp?)
   (rest-or or-list?))
  (and-exp
   (and symbol?)
   (left or-exp?)
   (right or-exp?))
  (or-exp
   (or symbol?)
   (left number?)
   (right number?))))

(define (and-list? x)
  (or (and (and-exp? (car x))
           (and-list? (cdr x)))
      (null? x)))

(define (or-list? x)
  (or (and (or-exp? (car x))
           (or-list? (cdr x)))
      (null? x)))

(define (and-exp? x)
  (and (list? x)
       (= (length x) 3)
       (eq? (car x) 'and)
       (or-exp? (cadr x))
       (or-exp? (caddr x)))))

(define (or-exp? x)
  (and (list? x)
       (= (length x) 3)
       (eq? (car x) 'or)
       (number? (cadr x))
       (number? (caddr x)))))

(provide (all-defined-out))
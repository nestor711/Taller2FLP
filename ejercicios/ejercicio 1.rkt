#lang eopl





#|
<FNC>      := ('fnc <int> <and-list>)

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
|#

; Constructores
(define (fnc n and-list)
  (list 'fnc n and-list))

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

; Crear instancias SAT
(define sat1 (fnc 3 (list (and-exp (or-exp (variable 1) (variable 2))
                                   (or-exp (variable 3) (variable 4))))))

(define sat2 (fnc 2 (list (or-exp (variable 1) (variable 2))
                           (or-exp (variable 3) (variable 4)))))

(define sat3 (fnc 4 (list (or-exp (variable 1) (variable 2))
                           (and-exp (or-exp (variable 3) (variable 4))
                                    (and-exp (variable 5) (variable 6))))))

#| Gramatica para los Datatypes
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

|#
; Cambiar los nombres de las funciones y tipos de datos para evitar conflictos


(define (make-fnc-exp n and-list)
  (list 'fnc n and-list))

(define (make-and-list exps)
  (cons 'and-list exps))

(define (make-or-list exps)
  (cons 'or-list exps))

(define (make-and-exp left right)
  (list 'and left right))

(define (make-or-exp left right)
  (list 'or left right))

(define (make-variable value)
  value)

(define (fnc-exp? x)
  (and (list? x)
       (= (length x) 3)
       (eq? (car x) 'fnc)
       (number? (cadr x))
       (and-list? (caddr x))))

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
       (or-exp? (caddr x))))

(define (or-exp? x)
  (and (list? x)
       (= (length x) 3)
       (eq? (car x) 'or)
       (number? (cadr x))
       (number? (caddr x))))

(provide (all-defined-out))





#|
;;casos de prueba exitoso
(define prueba1 (make-fnc-exp 3 (make-and-list (make-and-exp (make-or-exp (make-variable 1) (make-variable 2))
                                                            (make-or-exp (make-variable 3) (make-variable 4))))))

(display (fnc-exp? prueba1)) ; Debería imprimir #t si es una expresión FNC válida -->retorna falso

;caso de prueba que fracasa ¿por que ?
;caso con sat2
(define prueba2 (make-fnc-exp 2 (make-and-list (make-or-exp (make-variable 1) (make-variable 2))
                                                (make-or-exp (make-variable 3) (make-variable 4)))))

(display (fnc-exp? prueba2)) ; Debería imprimir #t si es una expresión FNC válida

;caso de prueba con  sat3
(define prueba3 (make-fnc-exp 4 (make-and-list (make-or-exp (make-variable 1) (make-variable 2))
                                                (make-and-exp (make-or-exp (make-variable 3) (make-variable 4))
                                                              (make-and-exp (make-variable 5) (make-variable 6))))))

(display (fnc-exp? prueba3)) ; Debería imprimir #t si es una expresión FNC válida

|#
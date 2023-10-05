#lang racket
; #lang eopl

;----------INTEGRANTES-------------- 
;
;Kevin Alejandro Velez Agudelo
;kevin.alejandro.velez@correounivalle.edu.co
;
;Moreno Romero Miguel Angel 
;miguel.romero@correounivalle.edu.co
;
;Nestor David Heredia Gutierrez
;nestor.heredia@correounivalle.edu.co

;; Importa funciones auxiliares 
(require "ejercicio1.rkt")
(require "ejercicio2.rkt")

;; Solucion de problemas SAT
;;
;; Ejercicio 3

;; generate-assignments
;; Proposito
;; int -> {<bool>}* : Genere todas las combinaciones de valores lógicos posibles para n variables y devuélvalas como una lista.

(define (generate-assignments n)
  (if (= n 0)
      '(())  ; Cuando n es 0, solo hay una asignación vacía.
      (let* ([rest (generate-assignments (- n 1))]
             [assignments (map (lambda (a) (cons #t a)) rest)]) ; Asignaciones con #t
             [assignments-false (map (lambda (a) (cons #f a)) rest)]) ; Asignaciones con #f
        (append assignments assignments-false) ; Concatenar las asignaciones con #t y #f
      )
  )


;; eval-literal (Evaluacón de Literales)
;; Proposito
;; int x {<bool>} -> bool: Determina si el valor de una variable es verdadero o falso.

(define (eval-literal literal assignment)
  (let ((var (abs literal)))
    (if (< literal 0) 
        (not (list-ref assignment (- var 1))) 
        (list-ref assignment (- var 1)))))

;; eval-clause (Evaluar Clausulas)
;; Proposito
;; or-list x {<bool>} -> bool: Dada una cláusula del tipo `or-list`, el objetivo es determinar si contiene al menos un valor verdadero 
;; cuando se evalúa utilizando una lista de valores booleanos asignados a cada variable. 
;; Para lograr esto, aplicamos la función `eval-literal` a cada elemento de la cláusula, a excepción del indicador `or`.
;;
;; <or-list>  := (<or-exp> <or-list>)
;;            := (<or-exp>)

(define (eval-clause clause assignment)
  ; Aplicamos ormap para evaluar cada literal en la cláusula
  (ormap 
   (lambda (literal)
     ; Llamamos a eval-literal para obtener el valor del literal
     (eval-literal literal assignment))
   ; Tomamos la lista de literales a partir del segundo elemento (cdr)
   (cdr clause)
  )
)

;; eval-fnc
;; Proposito
;; and-list x {<bool>} -> bool: El objetivo es determinar si todas las cláusulas del tipo `or-list` presentes en una lista son verdaderas. 
;; Para lograrlo, se aplica la función `eval-clause` a cada cláusula contenida en el elemento `and-list`.
;;
;; <and-list> := (<and-exp> <and-list>)
;;            := (<and-exp>)

; La función eval-fnc verifica si todas las cláusulas en fnc son verdaderas
(define (eval-fnc fnc assignment)
  ; Utilizamos andmap para verificar cada cláusula en fnc
  (andmap 
   (lambda (clause) 
     ; Evaluamos cada cláusula con la asignación dada
     (eval-clause clause assignment)) 
   fnc)
)

;; evaluarsat
;; Proposito
;; <FNC> -> {<bool>}: El objetivo es determinar si existe una combinación de valores que cumpla con una instancia `fnc` específica. 
;; Para lograrlo, primero se extraen las cláusulas del tipo `or-list` de la instancia utilizando el método previamente definido. 
;; Luego, se generan todas las posibles combinaciones de valores en la variable `assignments`. A continuación, se crea la función `solve`, la cual itera sobre la lista de asignaciones hasta que encuentre una combinación 
;; que satisfaga el problema o hasta que haya recorrido toda la lista.
;;
;; <FNC>      := ('fnc <int> <and-list>)
;; <and-list> := (<and-exp> <and-list>)
;;            := (<and-exp>)
;; <or-list>  := (<or-exp> <or-list>)
;;            := (<or-exp>)

(define (evaluarsat fnc)
  ; Extraemos las cláusulas y generamos las asignaciones iniciales
  (let* ([clauses (get-clauses fnc)]
         [assignments (generate-assignments (car fnc))])
    ; Definimos la función solve
    (let solve ([assignments assignments])
      (cond 
        ; Si no hay más asignaciones, el problema es insatisfactible
        [(null? assignments) 
         `(insatisfactible, assignments)]
        ; Si la evaluación de las cláusulas con la asignación actual es verdadera, el problema es satisfactible
        [(eval-fnc clauses (car assignments)) 
         `(satisfactible ,(car assignments))]
        ; Si no hemos agotado las asignaciones, seguimos probando con la siguiente
        [else 
         (solve (cdr assignments))]
      )
    )
  )
)

;; Pruebas
(define fnc1 (fnc-struct-list 4 (fnc-and-list (list
                (fnc-or-list '(1 -2 3 4))
                (fnc-or-list '(-2 3))
                (fnc-or-list '(-1 -2 -3))
                (fnc-or-list '(3 4))
                (fnc-or-list '(2))
                )))) ;;FNC 4 ( ( 1 or -2 or 3 or 4) and (-2 or 3) and(-1 or -2 or -3) and (3 or 4) and ( 2 ) )
(define fnc2 (fnc-struct-list 2 (fnc-and-list (list
                (fnc-or-list '(1 2))
                (fnc-or-list '(-1))
                (fnc-or-list '(-2))
                )))) ;;FNC 2 ( ( 1 or 2) and (-1) and (-2))

(evaluarsat fnc1)
(evaluarsat fnc2)

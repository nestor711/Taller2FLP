#lang eopl

;----------INTEGRANTES-------------- 
;
;Nestor David Heredia Gutierrez
;nestor.heredia@correounivalle.edu.co
;Código: 2058558
;
;Moreno Romero Miguel Angel 
;miguel.romero@correounivalle.edu.co
;Código: 2125737
;
;Kevin Alejandro Velez Agudelo
;kevin.alejandro.velez@correounivalle.edu.co
;Código: 2123281
;
;Repositorio: https://github.com/nestor711/Taller2FLP


(require "ejercicio1.rkt")
(require "ejercicio2.rkt")

;; Solucion de problemas SAT
;;
;; Ejercicio 3

;; generar-tareas
;; Proposito
;; int -> {<bool>}* : Genere todas las combinaciones de valores lógicos posibles para n variables y devuélvalas como una lista.
(define (generar-tareas n)
  ; La función generar-tareas toma un argumento n
  (if (= n 0)
      ; Si n es igual a cero, devuelve una lista que contiene una lista vacía '(())
      '(())
      ; En caso contrario
      (let* ([rest (generar-tareas (- n 1))]
             ; Se calcula la recursión con n-1 y se almacena en la variable rest
             [assignments (map (lambda (a) (cons #t a)) rest)])
             ; Se crea una lista assignments aplicando una función lambda a cada elemento de rest. 
             ; La función lambda agrega #t al comienzo de cada elemento de rest.
        (append assignments (map (lambda (a) (cons #f a)) rest)))
        ; Se agregan a assignments los resultados de aplicar una función lambda a cada elemento de rest, 
        ; donde se agrega #f al comienzo de cada elemento de rest.
  )
)

;; evaluar-literal (Evaluar Literales)
;; Proposito
;; int x {<bool>} -> bool: Determina si el valor de una variable es verdadero o falso.
(define (eval-literal literal assignment)
  ; Extraemos el número de variable de literal
  (let ((var (abs literal)))
    (if (< literal 0) 
        ; Si el literal es negativo, negamos el valor de la variable
        (not (list-ref assignment (- var 1))) 
        ; Si el literal es positivo, devolvemos el valor de la variable
        (list-ref assignment (- var 1)))))

;; eval-clause (Evaluar Clausulas)
;; Proposito
;; or-list x {<bool>} -> bool: Dada una cláusula del tipo `or-list`, el objetivo es determinar si contiene al menos un valor verdadero 
;; cuando se evalúa utilizando una lista de valores booleanos asignados a cada variable. 
;; Para lograr esto, aplicamos la función `eval-literal` a cada elemento de la cláusula, a excepción del indicador `or`.


; Definición de la función 'my-ormap'
; Esta función verifica si al menos un elemento de la lista satisface una condición dada por el procedimiento 'proc'.
(define (my-ormap proc lst)
  ; Si la lista está vacía, retorna #f, indicando que no se encontró ningún elemento que cumple con la condición.
  (cond ((null? lst) #f)
        ; Si el procedimiento 'proc' aplicado al primer elemento de la lista (car lst) retorna #t,
        ; entonces la función retorna #t, indicando que se encontró un elemento que cumple con la condición.
        ((proc (car lst)) #t)
        ; En caso contrario, se continúa la búsqueda en el resto de la lista (cdr lst) mediante una llamada recursiva.
        (else (my-ormap proc (cdr lst)))
  )
)

(define (eval-clause clause assignment)
  ; Aplicamos ormap para evaluar cada literal en la cláusula
  (my-ormap 
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


; Definición de la función 'my-andmap'
; Esta función verifica si todos los elementos de la lista satisfacen una condición dada por el procedimiento 'proc'.
(define (my-andmap proc lst)
  ; Si la lista está vacía, retorna #t, indicando que todos los elementos cumplen con la condición.
  (cond ((null? lst) #t)
        ; Si el procedimiento 'proc' aplicado al primer elemento de la lista (car lst) retorna #f,
        ; entonces la función retorna #f, indicando que al menos un elemento no cumple con la condición.
        ((not (proc (car lst))) #f)
        ; En caso contrario, se continúa la verificación en el resto de la lista (cdr lst) mediante una llamada recursiva.
        (else (my-andmap proc (cdr lst)))
  )
)

; La función eval-fnc verifica si todas las cláusulas en fnc son verdaderas
(define (eval-fnc fnc assignment)
  ; Utilizamos andmap para verificar cada cláusula en fnc
  (my-andmap 
   (lambda (clause) 
     ; Evaluamos cada cláusula con la asignación dada
     (eval-clause clause assignment)) 
   fnc)
)

;; EVALUARSAT
;; Proposito
;; <FNC> -> {<bool>}: El objetivo es determinar si existe una combinación de valores que cumpla con una instancia `fnc` específica. 
;; Para lograrlo, primero se extraen las cláusulas del tipo `or-list` de la instancia utilizando el método previamente definido. 
;; Luego, se generan todas las posibles combinaciones de valores en la variable `assignments`. A continuación, se crea la función `solve`,
;; la cual itera sobre la lista de asignaciones hasta que encuentre una combinación que satisfaga el problema o hasta que haya recorrido toda la lista.

(define (EVALUARSAT fnc)
  ; Extraemos las cláusulas y generamos las asignaciones iniciales
  (let* ([clauses (extract-lista-clausulas fnc)]
         [assignments (generar-tareas (car fnc))])
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
(EVALUARSAT (fnc-expression 4 (fnc-and-clausulas (list
                (fnc-or-lista '(1 -2 3 4))
                (fnc-or-lista '(-2 3))
                (fnc-or-lista '(-1 -2 -3))
                (fnc-or-lista '(3 4))
                (fnc-or-lista '(2))
                )))) ;;FNC 4 ( ( 1 or -2 or 3 or 4) and (-2 or 3) and(-1 or -2 or -3) and (3 or 4) and ( 2 ) )

(EVALUARSAT (fnc-expression 2 (fnc-and-clausulas (list
                (fnc-or-lista '(1 2))
                (fnc-or-lista '(-1))
                (fnc-or-lista '(-2))
                )))) ;;FNC 2 ( ( 1 or 2) and (-1) and (-2))

(EVALUARSAT (fnc-struct-list 3 (fnc-and-list (list
                (fnc-or-list '(1 -1))
                (fnc-or-list '(2 3))
                )))) ; FNC 3 ((1 or -1) and (2 or 3))

(evaluarsat fnc1)
(evaluarsat fnc2)

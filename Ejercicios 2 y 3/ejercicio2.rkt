#lang eopl

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



;; Importa funciones auxiliares del archivo ejercicio1.rkt
(require "ejercicio1.rkt")


#|<expresion> ::= <lista_vacia>
              | <simbolo>
              | <or-list>
              | <clausula>
              | <lista>

<lista_vacia> ::= ()

<simbolo> ::= <cadena>

<or-list> ::= (or-list <or-list_vars>)

<or-list_vars> ::= <expresion>
                 | <expresion> <or-list_vars>

<clausula> ::= (clausula <expresion>)

<lista> ::= (<expresion>)
          | (<expresion> <expresion>)
          | (<expresion> <lista>)
|#


(define (PARSEBNF expression)
  ; Función auxiliar que emula la funcionalidad de map aplicando una función a cada elemento de la lista
  (define (custom-map func lst)
    (if (null? lst)
        '()
        (cons (func (car lst)) (custom-map func (cdr lst)))))

  ; Función auxiliar que emula la funcionalidad de append concatenando dos listas
  (define (custom-append lst1 lst2)
    (if (null? lst1)
        lst2
        (cons (car lst1) (custom-append (cdr lst1) lst2))))

  ; Evaluación de la expresión utilizando cond para manejar varios casos
  (cond
    ; Si la expresión está vacía, devuelve una lista vacía
    ((null? expression) '())
    ; Si la expresión es un símbolo, devuelve una lista que contiene ese símbolo
    ((symbol? expression) (list expression))
    ; Si la expresión es una or-list, aplica custom-map a los elementos de la or-list y luego custom-append para concatenar los resultados
    ((eq? (car expression) 'or-list)
     (custom-append (custom-map PARSEBNF (or-list-vars expression)) '()))
    ; Si la expresión es una cláusula, llama recursivamente a PARSEBNF en su contenido directamente
    ((eq? (car expression) 'clausula) (PARSEBNF (cadr expression)))
    ; Si la expresión es una lista, aplica custom-map a los elementos de la lista y luego custom-append para concatenar los resultados
    ((pair? expression)
     (custom-append (custom-map PARSEBNF expression) '()))
    ; Para otros casos, devuelve una lista vacía
    (else '())))


#|<expresion> ::= <lista_vacia>
              | <literal>
              | <clausula>
              | <and-list>
              | <or-list>

<lista_vacia> ::= ()

<literal> ::= (literal <cadena>)

<clausula> ::= (clausula <or-list>)

<and-list> ::= (and-list <lista_de_expresiones>)

<lista_de_expresiones> ::= <expresion>
                          | <expresion> <lista_de_expresiones>

<or-list> ::= (or-list <lista_de_expresiones>)

<cadena> ::= <cualquier_secuencia_de_caracteres_valida>

|#

(define (UNPARSEBNF abstract-syntax-tree)
  ; Función auxiliar para procesar el árbol de sintaxis
  (define (unparse-helper tree)
    ; Función auxiliar para mapear una lista usando una función dada
    (define (custom-map func lst)
      (if (null? lst)
          '()
          (cons (func (car lst)) (custom-map func (cdr lst)))))

    ; Si el árbol está vacío, devuelve una lista vacía
    (if (null? tree)
        '()
        ; Si el árbol no está vacío, descompone el árbol y realiza las conversiones según las reglas establecidas
        (let ((first-elem (car tree))
              (rest-elems (cdr tree)))
          ; Comprueba el tipo del primer elemento del árbol y realiza la conversión correspondiente
          (cond
            ; Si el primer elemento es 'literal', convierte la variable literal y la agrega a la lista resultante
            ((eq? first-elem 'literal)
             (list 'literal (literal-variable (cadr tree))))
            ; Si el primer elemento es 'clausula', convierte la lista de variables 'or-list' y la agrega a la lista resultante
            ((eq? first-elem 'clausula)
             (list 'clausula (list 'or-list (custom-map (lambda (exp) (list 'literal exp)) (or-list-vars (cadr tree)))))) 
            ; Si el primer elemento es 'and-list', convierte la lista de expresiones 'and-list' y la agrega a la lista resultante
            ((eq? first-elem 'and-list)
             (list 'and-list (unparse-helper (and-list-exps (cadr tree)))))
            ; Si el primer elemento es 'or-list', convierte la lista de variables 'or-list' y la agrega a la lista resultante
            ((eq? first-elem 'or-list)
             (list 'or-list (custom-map (lambda (exp) (list 'literal exp)) (or-list-vars (cadr tree))))) 
            ; Si el primer elemento no coincide con ninguno de los casos anteriores, devuelve un mensaje de error
            (else (list "Estructura de árbol no válida"))))))
  ; Llama a la función auxiliar con el árbol de sintaxis abstracta y devuelve el resultado
  (unparse-helper abstract-syntax-tree))



;; Ejemplo de uso:
(display "PARSEBNF:")
(display (PARSEBNF '(clausula (or-list ((literal a) (literal b))))));deberia imprimir (((literal) (a)) ((literal) (b)))
(newline)
(newline)
(display "UNPARSEBNF:")
(display (UNPARSEBNF '(clausula (or-list ((a) (b)))))) ; Debería imprimir '(clausula (or-list ((literal a) (literal b))))'
(newline)



(define clausulaa1 (make-clausula (make-or-list (list (make-or-exp (literal-variable litt1) (literal-variable litt2))
                                                      (make-or-exp (literal-variable litt3) (literal-variable litt4))))))

(display "UNPARSEBNF:")
(display (UNPARSEBNF clausulaa1))








(provide (all-defined-out))



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


; BNF
; <fnc-expression> ::= "FNC" <num-variables> <lista-clausulas>
;  <num_variables> ::= <int>
;<lista-clausulas> ::= and (<clausula> <lista-clausulas>) | <clausula>  
;       <clausula> ::= or (<literal> <clausula>) | <literal>
;        <literal> ::= <int>


;Constructores

;Constructor de la funcion FNC
(define (fnc-expression num-variables and-clausulas) ;recibe un entero y una lista de and-clausulas
  (cons num-variables and-clausulas)) ;une el número de variables con la lista de and-clausulas
;Pruebas
;(fnc-expression 0 (fnc-and-clausulas '()))  salida: (0 and)

;(fnc-expression 3 (fnc-and-clausulas (list(fnc-or-lista '(2 3 1))))) salida: (3 and (or 2 3 1))

;(fnc-expression 4 (fnc-and-clausulas (list
;               (fnc-or-lista '(1 -2))
;               (fnc-or-lista '(2 2))
;               ))) salida: (4 and (or 1 -2) (or 2 2))



;Constructor del operador and
(define (fnc-and-clausulas lista-clausulas) ;recibe una lista de clausulas and 
    (cons 'and lista-clausulas)) ;retorna and con la lista de clausulas
;Pruebas
;(fnc-and-clausulas (list (fnc-or-lista '(1 2 3))))  salida: (and (or 1 2 3))

;(fnc-and-clausulas (list (fnc-or-lista '(3))))  salida: (and (or 3))

;(fnc-and-clausulas '()) salida: (and)



;Constructor del operador or
(define (fnc-or-lista int-list) ;recibe una lista de literales
  (cons 'or int-list)) ;retorna or junto a la lista de los literales
;Pruebas
;(fnc-or-lista '(1 2 3 4 5 1)) salida: (or 1 2 3 4 5 1)

;(fnc-or-lista '()) salida: (or)

;(fnc-or-lista '(2 2 2)) salida: (or 2 2 2)



; Extractores

; Extractor de la lista de literales
(define (extract-literales fnc-expression)
  (cadadr fnc-expression)) 
;Pruebas
;(extract-literales (fnc-expression 2 (fnc-and-clausulas (list (fnc-or-lista '(1 2 -1 -2))))))
;salida: (or 1 2 -1 -2)

;(extract-literales (fnc-expression 0 (fnc-and-clausulas (list (fnc-or-lista '())))))
;salida: (or)

;(extract-literales (fnc-expression 4 (fnc-and-clausulas (list (fnc-or-lista '(1 2 3 4))))))
;salida: (or 1 2 3 4)


; Extractor de la lista de clausulas
(define (extract-lista-clausulas fnc-expression)
  (cddr fnc-expression))
;Pruebas
;(extract-lista-clausulas (fnc-expression 2 (fnc-and-clausulas (list (fnc-or-lista '(1 2 -1))))))
;salida: ((or 1 2 -1))

;(extract-lista-clausulas (fnc-expression 4 (fnc-and-clausulas (list (fnc-or-lista '(1 2 3 4))))))
;salida: ((or 1 2 3 4))

;(extract-lista-clausulas (fnc-expression 0 (fnc-and-clausulas (list (fnc-or-lista '())))))
;salida: ((or))


; Extractor de la variable que indica el número de variables
(define (extract-int fnc-expression)
  (car fnc-expression))
;Pruebas
;(extract-int (fnc-expression 0 (fnc-and-clausulas (list (fnc-or-lista '())))))
;salida: 0

;(extract-int (fnc-expression 4 (fnc-and-clausulas (list (fnc-or-lista '(1 2 3 4))))))
;salida: int

;(extract-int (fnc-expression 2 (fnc-and-clausulas (list (fnc-or-lista '(1 2 -1))))))
;salida: 2


;Definición de datatypes

;BNF para los datatypes

; <fnc-expression>   ::=  <fnc>     (int <fnc-and-clausula>)

;                    ::=  <fnc-and-clausula>  :=  <empty-fnc-clausula>
;                                              :=  (fnc-or-lista <fnc-and-clausula>)

;                    ::=  <fnc-or-lista>  :=  <empty-fnc-literales>
;                                         :=  (int <fnc-or-list>)



;datatype fnc-or-list
(define-datatype fnc-or-list fnc-or-list? ;recibe una lista de literales
  (empty-fnc-literal) ;caso terminal
  (fnc-literal (literal number?)(rest-fnc-or-list fnc-or-list?))) ;comprueba que cada literal sea un número 

;Pruebas
;> (fnc-literal -1 (fnc-literal 1 (fnc-literal 2 (empty-fnc-literal))))
;Salida
; #(struct:fnc-literal
; -1
; #(struct:fnc-literal
;   1
;   #(struct:fnc-literal 2 #(struct:empty-fnc-literal))))

;> (empty-fnc-literal)
;Salida:
;#(struct:empty-fnc-literal)

;> (fnc-literal 1 (fnc-literal 2 (fnc-literal 3 (empty-fnc-literal))))
;Salida:
;#(struct:fnc-literal
; 1
; #(struct:fnc-literal
;   2
;   #(struct:fnc-literal 3 #(struct:empty-fnc-literal))))



;datatype fnc-and-clausulas
(define-datatype fnc-and-clausula fnc-and-clausula? ;recibe una lista de clausulas and
  (empty-fnc-clausula) ;caso terminal
  (and-clausula (clausula fnc-or-list?)(rest-fnc-and-clausulas fnc-and-clausula?))) ;comprueba que las listas sean or-list

;Pruebas
;> (and-clausula
;          (fnc-literal 3 (fnc-literal -1 (empty-fnc-literal)))
;          (empty-fnc-clausula))
;Salida:
;#(struct:and-clausula
; #(struct:fnc-literal
;   3
;   #(struct:fnc-literal -1 #(struct:empty-fnc-literal)))
; #(struct:empty-fnc-clausula))

;> (and-clausula
;  (fnc-literal 3 (fnc-literal -1 (fnc-literal 2 (empty-fnc-literal))))
;  (and-clausula
;    (fnc-literal 3 (fnc-literal -2 (empty-fnc-literal)))
;    (empty-fnc-clausula)))
;Salida:
;#(struct:and-clausula
;  #(struct:fnc-literal
;    3
;    #(struct:fnc-literal
;      -1
;      #(struct:fnc-literal 2 #(struct:empty-fnc-literal))))
;  #(struct:and-clausula
;    #(struct:fnc-literal
;      3
;      #(struct:fnc-literal -2 #(struct:empty-fnc-literal)))
;    #(struct:empty-fnc-clausula)))

;> (and-clausula
;      (fnc-literal 1 (empty-fnc-literal))
;      (and-clausula
;        (fnc-literal 2 (empty-fnc-literal))
;        (and-clausula
;          (fnc-literal 3 (empty-fnc-literal))
;          (empty-fnc-clausula))))
;Salida:
;#(struct:and-clausula
;  #(struct:fnc-literal 1 #(struct:empty-fnc-literal))
;  #(struct:and-clausula
;    #(struct:fnc-literal 2 #(struct:empty-fnc-literal))
;    #(struct:and-clausula
;      #(struct:fnc-literal 3 #(struct:empty-fnc-literal))
;      #(struct:empty-fnc-clausula))))


;datatype fnc-expression
(define-datatype fnc fnc? ;recibe una expresión FNC
  (fnc-exp (int number?)(and-list fnc-and-clausula?))) ;comprueba que el primer elemento sea un número y que el resto sea un and-list

;Pruebas
;> (fnc-exp 0 (empty-fnc-clausula))                          (empty-fnc-clausula)))
;Salida:
;#(struct:fnc-exp 0 #(struct:empty-fnc-clausula))

;> (fnc-exp 3
;          (and-clausula
;           (fnc-literal 1 (fnc-literal -1 (empty-fnc-literal)))
;           (and-clausula
;                            (fnc-literal 2 (fnc-literal 3 (empty-fnc-literal)))
;                            (empty-fnc-clausula))))
;Salida:
;#(struct:fnc-exp
;  2
;  #(struct:and-clausula
;    #(struct:fnc-literal
;      1
;      #(struct:fnc-literal -1 #(struct:empty-fnc-literal)))
;    #(struct:and-clausula
;      #(struct:fnc-literal
;        2
;        #(struct:fnc-literal -2 #(struct:empty-fnc-literal)))
;      #(struct:empty-fnc-clausula))))

;> (fnc-exp 2 (and-clausula (fnc-literal 1 (fnc-literal -1 (empty-fnc-literal)))
;                          (empty-fnc-clausula)))
;Salida:
;#(struct:fnc-exp
;  2
;  #(struct:and-clausula
;    #(struct:fnc-literal
;      1
;      #(struct:fnc-literal -1 #(struct:empty-fnc-literal)))
;    #(struct:empty-fnc-clausula)))



(provide (all-defined-out))




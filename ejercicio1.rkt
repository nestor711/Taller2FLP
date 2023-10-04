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


; BNF
; <fnc-expression> ::= "FNC" <num-variables> <lista-clausulas>
;  <num_variables> ::= <int>
;<lista-clausulas> ::= and (<clausula> <lista-clausulas>) | <clausula>  
;       <clausula> ::= or (<literal> <clausula>) | <literal>
;        <literal> ::= <int>


; <fnc-expression>   ::=  <literal-exp>   (<literal>)
;                    ::=  <clausula-exp>  (<literal> "or" <literal>)
;                    ::=  <lista-clausulas-exp>  (<clausula-exp> "and" <clasula-exp>)
;                    ::=  <program-exp> (<digito>) (<lista-clausulas-exp>
 


;Constructores

(define (fnc-expression num-variables and-clausulas)
  (cons num-variables and-clausulas)) ;constructor de la funcion FNC
;Pruebas
;(fnc-expression 0 (fnc-and-clausulas '()))  salida: (0 and)



(define fnc-and-clausulas
  (lambda (lista-clausulas)
    (cons 'and lista-clausulas))) ;constructor del operador and
;Pruebas
;



(define (fnc-or-lista int-list)
  (cons 'or int-list)); constructor del operador or
;Pruebas
;




; Extractores

(define (extract-literales fnc-expression)
  (cadadr fnc-expression)) ; extractor de la lista de literales
;Pruebas
;

(define (extract-lista-clausulas fnc-expression)
  (cddr fnc-expression)); extractor de la lista de clausulas
;Pruebas
;(extract-lista-clausulas (fnc-expression 2 (fnc-and-clausulas (list (fnc-or-lista '(1 2 -1 -2))))))

(define (extract-int fnc-expression)
  (car fnc-expression)); extractor de la variable
;Pruebas
;


;Definición de datatypes

;BNF para los datatypes

; <fnc-expression>   ::=  <fnc-expression>     (int fnc-and-clausulas)

;                    ::=  <fnc-and-clausulas>  :=  <empty-fnc-clausula>
;                                              :=  (fnc-or-lista <fnc-and-clausulas>)

;                    ::=  <fnc-or-lista>  :=  <empty-fnc-literales>
;                                         :=  (int <fnc-or-list>)


(define-datatype fnc-or-list fnc-or-list?
  (empty-fnc-literales)
  (fnc-literales (literal number?) (rest-fnc-or-list fnc-or-list?)))

;Pruebas
; 
(define-datatype expression expression?
  (literal-exp
   (literal number?))
  (clausula-exp
   (rator expression?)
   (or symbol?)
   (rand expression?))
  (lista-clausula-exp
   (rator expression?)
   (and-logic symbol?)
   (rand expression?))
  (program-exp
   (num-variable number?)
   (body expression?)))

;Ejemplos:
; > (lista-clausula-exp (
;                      clausula-exp
;                      (literal-exp 1) 'or (literal-exp 2))
;                     'and
;                     (
;                      clausula-exp
;                       (literal-exp 3) 'or (literal-exp 4)))

; ->  #(struct:lista-clausula-exp
;       #(struct:clausula-exp
;         #(struct:literal-exp 1)
;         or
;         #(struct:literal-exp 2))
;       and
;       #(struct:clausula-exp
;         #(struct:literal-exp 3)
;         or
;         #(struct:literal-exp 4)))


;  > (clausula-exp (literal-exp 1) 'and (literal-exp 2))

; -> #(struct:clausula-exp
;      #(struct:literal-exp 1)
;      or
;      #(struct:literal-exp 2))


;  > literal-exp
; -> #<procedure:literal-exp>


;(define-type fnc-expression
  ;[make-FNC num-variables lista-clausulas]
  ;[FNC num-variables lista-clausulas])

; Tipo de dato para una expresión FNC
;(define-datatype fnc-expression fnc?
  ;(make-FNC 
   ; num-variables ; Número de variables en la expresión
    ;lista-clausulas)) ; Lista de cláusulas

; Tipo de dato para una lista de cláusulas
;(define-datatype lista-clausulas list?
  ;(make-lista-clausulas 
    ;clausula ; Primera cláusula
    ;lista-clausulas)) ; Resto de la lista de cláusulas

; Tipo de dato para una cláusula
;(define-datatype clausula fnc?
  ;(make-clausula 
    ;literal)) ; Literal que compone la cláusula

; Tipo de dato para un literal (que puede ser un entero o una variable)
;(define-datatype literal (int variable) 
  ;(make-variable 
   ; variable) ; Variable que representa el literal
  ;(make-int-literal 
   ; int ; Valor entero del literal
   ; variable)) ; Variable asociada al literal




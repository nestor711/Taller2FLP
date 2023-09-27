#lang eopl

; BNF
; <fnc-expression> ::= "FNC" <num-variables> <lista-clausulas>
;  <num_variables> ::= <int>
;<lista-clausulas> ::= <clausula> and <lista-clausulas> | <clausula>  
;       <clausula> ::= <literal> "or" <clausula> | <literal>
;        <literal> ::= <variable> | <int> <variable> 


; <fnc-expression>   ::=  <literal-exp>   (<literal>)
;                    ::=  <clausula-exp>  (<literal> "or" <literal>)
;                    ::=  <lista-clausulas-exp>  (<clausula-exp> "and" <clasula-exp>)
;                    ::=  <program-exp> (<digito>) (<lista-clausulas-exp>
 


;Constructores
(define (fnc-expression fnc num-variables list-clausulas)
  (list fnc num-variables list-clausulas)) ;constructor de la funcion FNC

(define (and-logic clausula1 clausula2)
  list clausula1 'and clausula2) ;constructor del operador and

(define (or-logic literal1 literal2)
  (list literal1 'or literal2)); constructor del operador or

; Extractores
(define (extract-list-clausulas fnc-expression)
  (cadr fnc-expression)); extractor de la lista de clausulas 

(define (extract-clausulas fnc-expression)
  (caddr fnc-expression)); extractor de los literales


;Definición de datatypes

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

;(define-datatype lc-exp lc-exp?
  ; (var-exp (id symbol?) )
   ;(lambda-exp (bound-var symbol?) (body lc-exp?) )
   ;(app-exp (rator lc-exp?) (rand lc-exp?) ) )

;(define datatype fnc-expression fnc?
  ;(fnc (num-variables int)(list-clausulas list)))

;(define datatype num-variables num-variables?
  ;((num-variables int?)
  ; list-clausulas?))

;(define gramatica
;'((white-sp
;   (whitespace) skip)
; (comment
 ;  ("//" (arbno (not #\newline))) skip)
 ; (identifier
 ;  (letter (arbno (or letter digit "?"))) symbol)
 ; (number
 ;  (digit (arbno digit)) number)
 ; (number
 ;  ("-" digit (arbno digit)) number)))


#lang eopl

; BNF
;<fnc-expression> ::= "FNC" <num-variables> <lista-clausulas>
;<num_variables> ::= <int>
;<lista-clausulas> ::= <clausula> and <lista-clausulas> | <clausula>  
;<clausula> ::= <literal> "or" <clausula> | <literal>
;<literal> ::= <variable> | <int> <variable> 

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


;datatypes
;(define datatype fnc-expression fnc?
  ;(fnc (num-variables int)(list-clausulas list)))

;(define datatype num-variables num-variables?
  ;((num-variables int?)
  ; list-clausulas?))

;Definición de datatypes
(define-datatype fnc-expression fnc?
  (make-FNC num-variables lista-clausulas))
(define-datatype lista-clausulas list?
  (make-lista-clausulas clausula lista-clausulas))
(define-datatype clausula fnc?
  (make-clausula literal))
(define-datatype literal (int variable) 
  (make-variable variable)
  (make-int-literal int variable))

(define gramatica
'((white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)))

(define-datatype lc-exp lc-exp?
   (var-exp (id symbol?) )
   (lambda-exp (bound-var symbol?) (body lc-exp?) )
   (app-exp (rator lc-exp?) (rand lc-exp?) ) )

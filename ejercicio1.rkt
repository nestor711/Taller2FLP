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


;; empty-env
(define empty-env
  (lambda () (list 'empty-env)))

;; extend-env
(define fnc
  (lambda (var lista)
    (list 'extend-env var val env)))

;; apply-env
(define apply-env
  (lambda (env search-var)
    (cond ((eqv? (car env) 'empty-env)
           (eopl:error 'apply-env "No binding for ~s" search-var))
          ((eqv? (car env) 'extend-env)
           (let ((saved-var (cadr env))
                 (saved-val (caddr env))
                 (saved-env (cadddr env)))
             (if (eqv? search-var saved-var)
                 saved-val
                 (apply-env saved-env search-var))))
          (else (eopl:error 'apply-env "Expecting an environment, given ~s" env)))))

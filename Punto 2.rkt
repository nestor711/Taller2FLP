#lang eopl
(require "ejercicio1.rkt")

(define (PARSEBNF tokens)
  (define (parse-FNC tokens)
    (match tokens
      [`(fnc ,num . ,rest) 
       (let* ((and-list-result (parse-and-list rest)))
         `(FNC ,num ,and-list-result))]
      (_ (error "Error de sintaxis en FNC"))))
  
  (define (parse-and-list tokens)
    (match tokens
      [`(() . ,rest) '()]
      [`((and . ,rest1) . ,rest2)
       (let* ((and-exp-result (parse-and-exp rest1))
              (and-list-result (parse-and-list rest2)))
         (cons and-exp-result and-list-result))]
      (_ (error "Error de sintaxis en and-list"))))
  
  (define (parse-or-list tokens)
    (match tokens
      [`(() . ,rest) '()]
      [`((or . ,rest1) . ,rest2)
       (let* ((or-exp-result (parse-or-exp rest1))
              (or-list-result (parse-or-list rest2)))
         (cons or-exp-result or-list-result))]
      (_ (error "Error de sintaxis en or-list"))))
  
  (define (parse-and-exp tokens)
    (match tokens
      [`((and . ,rest1) . ,rest2)
       (let* ((or-exp1-result (parse-or-exp rest1))
              (or-exp2-result (parse-or-exp (car rest2))))
         `(AND ,or-exp1-result ,or-exp2-result))]
      (_ (error "Error de sintaxis en and-exp")))
  
  (define (parse-or-exp tokens)
    (match tokens
      [`((or . ,var1 . ,var2) . ,rest)
       `(OR ,var1 ,var2)]
      (_ (error "Error de sintaxis en or-exp"))))
  
  (define (parse-variable tokens)
    (match tokens
      [`(,var . ,rest) 
       (if (positive? var)
           (cons var rest)
           (error "Error de sintaxis en variable"))]
      (_ (error "Error de sintaxis en variable"))))
  
  (let* ((ast (parse-FNC tokens))
         (rest-tokens (cddr ast)))
    (if (null? rest-tokens)
        (car ast)
        (error "Error: tokens adicionales no analizados"))))
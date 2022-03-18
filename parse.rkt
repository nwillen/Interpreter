#lang racket

;; To use this module, save this code as parse.rkt in the same directory as your
;; code and add the following line to your file:
;;
;; (require (only-in (file "parse.rkt") parse))

(provide parse)

(require (only-in (file "lex.rkt") lex))

;; A7 grammar:
;;
;; program       := exprList
;; exprList      := expr optExprList
;; optExprList   := ɛ | exprList
;; expr          := atom | invocation | let | define | lambda | class | new | send | super | set
;; class         := CLASS NAME OPAREN optNameList CPAREN OBRACE optMethodList CBRACE
;; new           := NEW NAME OPAREN optExprList CPAREN
;; send          := SEND NAME NAME OPAREN optExprList CPAREN
;; super         := SUPER OPAREN optExprList CPAREN
;; set           := SET NAME expr
;; optNameList   := ɛ | nameList
;; nameList      := NAME optNameList
;; optMethodList := ɛ | methodList
;; methodList    := method optMethodList
;; method        := NAME OPAREN optNameList CPAREN OBRACE exprList CBRACE
;; let           := LET OPAREN NAME expr CPAREN expr
;; define        := DEFINE NAME expr
;; lambda        := LAMBDA OPAREN NAME CPAREN expr
;; atom          := NAME | STRING | number
;; number        := INT | FLOAT
;; invocation    := OPAREN exprList CPAREN

(module+ test
  (require (only-in rackunit
                    check-equal?
                    check-exn
                    check-not-exn)))

(define tokens (make-parameter null))

(define DEBUG #f)
(define (trace label)
  (when DEBUG
    (writeln (~a label " "
                 (if (empty? (tokens))
                     'null
                     (first (tokens)))))))

(define (check type)
  (if (empty? (tokens))
      #f
      (equal? type (first (first (tokens))))))

(module+ test
  (check-equal? (parameterize ([tokens null])
                  (check 'DEF))
                #f)
  (check-equal? (parameterize ([tokens (list (list 'DEF #f))])
                  (check 'DEF))
                #t)
  (check-equal? (parameterize ([tokens (list (list 'DEF #f))])
                  (check 'FUN))
                #f))

(define (consume type)
  (when (empty? (tokens))
    (error (~a "expected token of type " type " but no remaining tokens")))
  (let ([token (first (tokens))])
    (when (not (equal? type (first token)))
      (error (~a "expected token of type " type " but actual token was " token)))
    (tokens (rest (tokens)))
    (when DEBUG
      (displayln (~a "(consume '" token ")")))
    token))

(module+ test
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens null])
                 (consume 'DEF))))
  (check-not-exn (lambda ()
                   (parameterize ([tokens (list (list 'DEF #f))])
                     (consume 'DEF))))
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens (list (list 'DEF #f))])
                 (consume 'FUN)))))

(define (parse-number)
  ;; number := INT | FLOAT
  (list 'number
        (if (check 'INT)
            (consume 'INT)
            (consume 'FLOAT))))

(define (number-pending?)
  (or (check 'INT)
      (check 'FLOAT)))

(module+ test
  (check-equal? (parameterize ([tokens (list (list 'INT 7))])
                  (parse-number))
                (list 'number (list 'INT 7)))
  (check-equal? (parameterize ([tokens (list (list 'FLOAT 7.7))])
                  (parse-number))
                (list 'number (list 'FLOAT 7.7)))
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens (list (list 'DEF #f))])
                 (parse-number)))))

(define (parse-atom)
  ;; atom := NAME | STRING | number
  (list 'atom
        (cond
          [(check 'NAME) (consume 'NAME)]
          [(check 'STRING) (consume 'STRING)]
          [else (parse-number)])))

(define (atom-pending?)
  (or (check 'NAME)
      (check 'STRING)
      (number-pending?)))

(module+ test
  (check-equal? (parameterize ([tokens (list (list 'NAME 'foo))])
                  (parse-atom))
                (list 'atom (list 'NAME 'foo)))
  (check-equal? (parameterize ([tokens (list (list 'STRING "foo"))])
                  (parse-atom))
                (list 'atom (list 'STRING "foo")))
  (check-equal? (parameterize ([tokens (list (list 'INT 7))])
                  (parse-atom))
                (list 'atom (list 'number (list 'INT 7))))
  (check-equal? (parameterize ([tokens (list (list 'FLOAT 7.7))])
                  (parse-atom))
                (list 'atom (list 'number (list 'FLOAT 7.7))))
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens (list (list 'DEF #f))])
                 (parse-atom)))))

(define (parse-invocation)
  ;; invocation := OPAREN exprList CPAREN
  (list 'invocation
        (consume 'OPAREN)
        (parse-expr-list)
        (consume 'CPAREN)))

(define (parse-expr)
  ;; expr := atom | invocation | let | define | lambda | class | new | send | super | set
  (list 'expr
        (cond
          [(atom-pending?) (parse-atom)]
          [(check 'OPAREN) (parse-invocation)]
          [(check 'LET) (parse-let)]
          [(check 'DEFINE) (parse-define)]
          [(check 'LAMBDA) (parse-lambda)]
          [(check 'CLASS) (parse-class)]
          [(check 'NEW) (parse-new)]
          [(check 'SEND) (parse-send)]
          [(check 'SUPER) (parse-super)]
          [else (parse-set)])))

(define (parse-let)
  ;; let := LET OPAREN NAME expr CPAREN expr
  (list 'let
        (consume 'LET)
        (consume 'OPAREN)
        (consume 'NAME)
        (parse-expr)
        (consume 'CPAREN)
        (parse-expr)))

(define (parse-define)
  ;; define := DEFINE NAME expr
  (list 'define
        (consume 'DEFINE)
        (consume 'NAME)
        (parse-expr)))

(define (parse-lambda)
  ;; lambda := LAMBDA OPAREN NAME CPAREN expr
  (list 'lambda
        (consume 'LAMBDA)
        (consume 'OPAREN)
        (consume 'NAME)
        (consume 'CPAREN)
        (parse-expr)))

(define (expr-pending?)
  ;; expr := atom | invocation | let | define | lambda | class | new | send | super | set
  (or (atom-pending?)
      (check 'OPAREN)
      (check 'LET)
      (check 'DEFINE)
      (check 'LAMBDA)
      (check 'CLASS)
      (check 'NEW)
      (check 'SEND)
      (check 'SUPER)
      (check 'SET)))

(define (parse-expr-list)
  ;; exprList := expr optExprList
  (list 'exprList
        (parse-expr)
        (parse-opt-expr-list)))

(define (expr-list-pending?)
  (expr-pending?))

(define (parse-opt-expr-list)
  ;; optExprList := ɛ | exprList
  (if (expr-list-pending?)
      (list 'optExprList (parse-expr-list))
      (list 'optExprList)))

(define (parse-name-list)
  ;; nameList := NAME optNameList
  (list 'nameList (consume 'NAME) (parse-opt-name-list)))

(define (parse-opt-name-list)
  ;; optNameList := ɛ | nameList
  (if (check 'NAME)
      (list 'optNameList (parse-name-list))
      (list 'optNameList)))

(define (parse-method-list)
  ;; methodList := method optMethodList
  (list 'methodList (parse-method) (parse-opt-method-list)))

(define (parse-opt-method-list)
  ;; optMethodList := ɛ | methodList
  (if (method-pending?)
      (list 'optMethodList (parse-method-list))
      (list 'optMethodList)))

(define (method-pending?)
  (check 'NAME))

(define (parse-method)
  ;; method := NAME OPAREN optNameList CPAREN OBRACE exprList CBRACE
  (list 'method
        (consume 'NAME)
        (consume 'OPAREN)
        (parse-opt-name-list)
        (consume 'CPAREN)
        (consume 'OBRACE)
        (parse-expr-list)
        (consume 'CBRACE)))

(define (parse-class)
  ;; class := CLASS NAME OPAREN optNameList CPAREN OBRACE optMethodList CBRACE
  (list 'class
        (consume 'CLASS)
        (consume 'NAME)
        (consume 'OPAREN)
        (parse-opt-name-list)
        (consume 'CPAREN)
        (consume 'OBRACE)
        (parse-opt-method-list)
        (consume 'CBRACE)))

(define (parse-new)
  ;; new := NEW NAME OPAREN optExprList CPAREN
  (list 'new
        (consume 'NEW)
        (consume 'NAME)
        (consume 'OPAREN)
        (parse-opt-expr-list)
        (consume 'CPAREN)))

(define (parse-send)
  ;; send := SEND NAME NAME OPAREN optExprList CPAREN
  (list 'send
        (consume 'SEND)
        (consume 'NAME)
        (consume 'NAME)
        (consume 'OPAREN)
        (parse-opt-expr-list)
        (consume 'CPAREN)))

(define (parse-super)
  ;; super := SUPER OPAREN optExprList CPAREN
  (list 'super
        (consume 'SUPER)
        (consume 'OPAREN)
        (parse-opt-expr-list)
        (consume 'CPAREN)))

(define (parse-set)
  ;; set := SET NAME expr
  (list 'set
        (consume 'SET)
        (consume 'NAME)
        (parse-expr)))

(define (parse-program)
  ;; program := exprList
  (list 'program
        (parse-expr-list)))

(define (parse code)
  (parameterize ([tokens (lex code)])
    (parse-program)))




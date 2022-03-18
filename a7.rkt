#lang racket

(require "parse.rkt")

;; Environment
(define new-environment make-hash)
(define new-env-from hash-copy)
(define (add-binding env name value)
  (hash-set! env name value)
  env)
(define lookup-name hash-ref)
(define binding-exists? hash-has-key?)
(define (update-binding env name value)
  (if (binding-exists? env name)
    (add-binding env name value)
    (error "Cannot update nonexistent name " name)))
(define (add-bindings env names values)
  (foldl (λ (n v e) (add-binding e n v))
         env names values))

;;Class Representation
(define (Class superclass field-names method-table defining-env)
  (list superclass field-names method-table defining-env))
(define class.superclass first)
(define class.fields second)
(define class.method-table third)
(define class.env fourth)
(define (collect-field-names class)
  (let ([superclass (class.superclass class)]
        [fields (class.fields class)])
    (if superclass
      (remove-duplicates (append fields (collect-field-names superclass)))
      fields)))

;;Method Representation
(define (method params body-exprs) (list params body-exprs))
(define method.params first)
(define method.body-exprs second)

;;Object Representation
(define (object class env) (list class env))
(define object.class first)
(define object.env second)

(define (base-object)
  (Class #f null
    `#hash((initialize . (,null (exprList (expr (atom (number (INT 0)))) (optExprList)))))
    (new-environment)))


(define (base-env)
  (add-binding (new-environment) 'Object (base-object)))

(define (eval code-string)
  (eval-program (parse code-string) (base-env)))

(define (eval-program program-expr env)
  ;; program     := exprList
  (last (eval-expr-list (second program-expr) env)))

(define (eval-expr-list exprList-expr env)
  ;; exprList    := expr optExprList
  (let* ([expr-expr (second exprList-expr)]
         [expr-tag (first (second expr-expr))]
         [optExprList-expr (third exprList-expr)])
    (if (equal? expr-tag 'define)
      ;; define      := DEFINE NAME expr
      (let* ([define-expr (second expr-expr)]
             [name (second (third define-expr))]
             [value-expr (fourth define-expr)]
             [new-env (add-binding env name (eval-expr value-expr env))])
        (eval-opt-expr-list (lookup-name new-env name)
                          optExprList-expr
                          new-env))
      ;; normal stuff
      (eval-opt-expr-list (eval-expr expr-expr env)
                        optExprList-expr
                        env))))

(define (eval-opt-expr-list value optExprList-expr env)
  ;; optExprList := ɛ | exprList
  (cons value (if (empty? (rest optExprList-expr))
                null
                (eval-expr-list (second optExprList-expr) env))))

(define (eval-expr expr-expr env)
  ;; expr := atom | invocation | let | define | lambda | class | new | send | super | set
  (let* ([expr-to-eval (second expr-expr)]
         [tag (first expr-to-eval)]
         [evaluator (case tag
                      [(atom) eval-atom]
                      [(invocation) eval-invocation]
                      [(let) eval-let]
                      ;; define case is handled in `eval-expr-list`
                      [(lambda) eval-lambda]
                      [(class) eval-class]
                      [(new) eval-new]
                      [(send) eval-send]
                      [(super) eval-super])])
    (evaluator expr-to-eval env)))

(define (eval-atom atom-expr env)
  ;; atom        := NAME | STRING | number
  (let* ([name-string-number (second atom-expr)]
         [tag (first name-string-number)]
         [evaluator (case tag
                      [(NAME) eval-name]
                      [(STRING) eval-string]
                      [(number) eval-number])])
    (evaluator name-string-number env)))

(define (eval-name name-expr env)
  ;; + - * / string-append string<? string=? not = <
  (case (second name-expr)
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(string-append) string-append]
    [(string<?) string<?]
    [(string=?) string=?]
    [(not) not]
    [(=) =]
    [(<) <]
    [else (lookup-name env (second name-expr))]))

(define (eval-string string-expr env) (second string-expr))
(define (eval-number number-expr env)
  ;; number      := INT | FLOAT
  (second (second number-expr)))

(define (eval-let let-expr env)
  ;; let         := LET OPAREN NAME expr CPAREN expr
  (let* ([name (second (fourth let-expr))]
         [value-expr (fifth let-expr)]
         [body-expr (seventh let-expr)])
    (eval-expr body-expr
               (add-binding env name (eval-expr value-expr env)))))

(define (eval-lambda lambda-expr env)
  ;; lambda      := LAMBDA OPAREN NAME CPAREN expr
  (let* ([name (second (fourth lambda-expr))]
         [body-expr (sixth lambda-expr)])
    (lambda (value)
      (eval-expr body-expr
                 (add-binding env name value)))))

(define (eval-invocation invocation-expr env)
  ;; invocation  := OPAREN exprList CPAREN
  (let* ([exprList-expr (third invocation-expr)]
         [rator-expr (second (second exprList-expr))]
         [values (eval-expr-list exprList-expr env)]
         [rator (first values)]
         [rands (rest values)])
    (apply rator rands)))

(define (opt*list->list opt*list-expr)
  (if (empty? (rest opt*list-expr))
    null
    (*list->list (second opt*list-expr))))

(define (*list->list *list-expr)
  (cons (second *list-expr)
        (opt*list->list (third *list-expr))))

(define (eval-class class-expr env)
  ;;class := CLASS NAME OPAREN optNameList CPAREN OBRACE optMethodList CBRACE
  (let*  ([superclass-name (second (third class-expr))]
          [superclass (lookup-name env superclass-name)]
          [opt-name-list-expr (fifth class-expr)]
          [name-list-expr (opt*list->list opt-name-list-expr)]
          [names (map second name-list-expr)]
          [opt-method-list-expr (eighth class-expr)]
          [method-list-expr (opt*list->list opt-method-list-expr)]
          [method-table (make-method-table method-list-expr)]
          [def-env (new-env-from env)]
          [class (Class names def-env method-table superclass)])
    class))

(define (make-method-table method-list)
  ;; method := NAME OPAREN optNameList CPAREN OBRACE exprList CBRACE
  (let ([method-table-entry
          (λ (method-expr)
            (let* ([method-name (second (second method-expr))]
                   [params-opt-list (fourth method-expr)]
                   [params-list (opt*list->list params-opt-list)]
                   [params (map second params-list)]
                   [body-expr (seventh method-expr)])
              (cons method-name (method params body-expr))))])
    (make-hash (map method-table-entry method-list))))
(define method-defined? hash-has-key?)
(define get-method hash-ref)

(define (eval-new new-expr env)
  ;; new := NEW NAME OPAREN optExprList CPAREN
  (let* ([class (lookup-name env (second (third new-expr)))]
         [fields (collect-field-names class)]
         [obj-env (add-bindings (new-env-from (class.env class))
                                   field-names
                                   (map (const (void)) fields))]
         [obj (object class obj-env)]
         [opt-expr-list (fifth new-expr)]
         [expr-list (opt*list->list opt-expr-list)])
    (call-method obj 'initialize expr-list env)
    obj))

(define (call-method object method-name expr-list env)
  (let* ([class (object.class object)]
         [method-class (find-method-or-fail class method-name)])
    (call-method-directly method-class object method-name expr-list env)))

(define (call-method-directly class obj method-name expr-list-expr env)
   (let* ([method-table (class.method-table class)]
          [method (get-method method-table method-name)]
          [params (method.params method)]
          [body-exprs (method.body-exprs method)]

          [args (map (λ (e) (eval-expr e env)) expr-list-expr)]

          [object-env (object.env object)]
          [env-with-this (add-binding object-env 'this object)]
          [env-with-args (add-bindings env-with-this params args)]

          [superclass (class.superclass class)]
          [env-with-super (add-binding env-with-args 'super (super-method method-name superclass))])
    (last (eval-expr-list body-exprs env-with-args))))

(define (super-method name class) (list name class))
(define super.method-name first)
(define super.method-class second)

(define (find-method-or-fail class method-name)
  (or (find-method class method-name)
      (error (~a "Method not found: " method-name))))

(define (find-method class method-name)
  (and class
      #f
      (let ([superclass (class.superclass class)]
            [method-table (class.method-table)])
        (if (method-defined? method-table method-name)
            class
            (find-method superclass method-name)))))


(define (eval-send send-expr env)
  ;;send := SEND NAME NAME OPAREN optExprList CPAREN
  (let* ([obj (lookup-name env (second (third send-expr)))]
         [method-name (second (fourth send-expr))]
         [opt-expr-list-expr (sixth send-expr)]
         [expr-list-expr (opt*list->list opt-expr-list-expr)])
    (call-method obj method-name expr-list-expr env)))

(define (eval-super super-expr env)
  ;;super := SUPER OPAREN optExprList CPAREN
  (let* ([obj (lookup-name env 'this)]
         [super-method (lookup-name env 'super)]
         [method-name (super.method-name super-method)]
         [current-class (super.method-class super-method)]
         [method-class (find-method-or-fail current-class method-name)]
         [opt-expr-list-expr (fourth super-expr)]
         [expr-list-expr (opt*list->list opt-expr-list-expr)])
    (call-method-directly method-class obj method-name expr-list-expr env)))


(define (eval-set set-expr env)
  ;;set := SET NAME expr
  (let* ([name (second (third set-expr))]
         [expr-expr (fourth set-expr)]
         [expr (eval-expr expr-expr env)])
    (update-binding env name expr)
    expr))


(parse "define Box
  class Object (x) {
    initialize(x0) {
      set x x0
    }
    reset(new-x) {
      set x new-x
    }
    fetch() {
      x
    }
  }

define NumberBox
  class Box (x) {
    initialize(x0) {
      super(x0)
    }
    add1() {
      set x (+ x 1)
    }
    sub1() {
      set x (- x 1)
    }
  }

define AdderBox
  class NumberBox (x) {
    initialize(x0) {
      super(x0)
    }
    add(y) {
      set x (+ x y)
    }
    sub(y) {
      send this add((- y))
    }
  }

define StatelessAdder
  class Object () {
    add2(x y) {
      let (box1 new AdderBox(x))
       send box1 add(y)
    }
  }

define adder new StatelessAdder()
send adder add2(3 39)")

(display "\n")

(eval "define Box
  class Object (x) {
    initialize(x0) {
      set x x0
    }
    reset(new-x) {
      set x new-x
    }
    fetch() {
      x
    }
  }

define NumberBox
  class Box (x) {
    initialize(x0) {
      super(x0)
    }
    add1() {
      set x (+ x 1)
    }
    sub1() {
      set x (- x 1)
    }
  }

define AdderBox
  class NumberBox (x) {
    initialize(x0) {
      super(x0)
    }
    add(y) {
      set x (+ x y)
    }
    sub(y) {
      send this add((- y))
    }
  }

define StatelessAdder
  class Object () {
    add2(x y) {
      let (box1 new AdderBox(x))
       send box1 add(y)
    }
  }

define adder new StatelessAdder()
send adder add2(3 39)")





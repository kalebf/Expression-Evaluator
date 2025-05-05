#lang racket

;; Define our own Maybe and Either types
(define nothing (list 'nothing))
(define (just x) (list 'just x))
(define (nothing? x) (equal? x nothing))
(define (just? x) (and (pair? x) (equal? (car x) 'just)))
(define (from-just default x)
  (if (just? x) (cadr x) default))

(define (success x) (list 'success x))
(define (failure x) (list 'failure x))
(define (success? x) (and (pair? x) (equal? (car x) 'success)))
(define (failure? x) (and (pair? x) (equal? (car x) 'failure)))
(define (from-success default x)
  (if (success? x) (cadr x) default))
(define (from-failure default x)
  (if (failure? x) (cadr x) default))

;; Helper functions
(define (in-list? x lst)
  (not (false? (member x lst))))

;; State representation (list of pairs (id . value))
(define (initial-state) '())

;; Helper to look up a variable in state
(define (lookup-var id state)
  (let ((result (assoc id state)))
    (cond
      [(not result) (failure (list "Error: variable not defined" id))]
      [(equal? (cdr result) 'undefined) (failure (list "Error: variable is undefined" id))]
      [else (success (cdr result))])))

;; Helper to add/update a variable in state
(define (update-var id value state)
  (if (assoc id state)
      (cons (cons id value) (remove (assoc id state) state))
      (cons (cons id value) state)))

;; Helper to remove a variable from state
(define (remove-var id state)
  (if (assoc id state)
      (remove (assoc id state) state)
      (begin
        (displayln (format "Error: remove ~a: variable not defined, ignoring" id))
        state)))

;;Variable Name Validation
(define (valid-id? id)
  (and (symbol? id)
       (let ((s (symbol->string id)))
         (and (char-alphabetic? (string-ref s 0))
              (regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9_-]*$" s)))))

;; Safe division now returns Either
(define (safe-div x y)
  (if (= y 0)
      (failure "Error: division by zero")
      (success (/ x y))))

;; Main evaluator function
(define (eval expr state)
  (cond  
    [(equal? (first expr) 'remove)
 (values (success (second expr)) (remove-var (second expr) state))]
    
    [(equal? (first expr) 'num) (values (success (second expr)) state)]
    
    [(equal? (first expr) 'id) 
     (let ((result (lookup-var (second expr) state)))
       (values result state))]
    
    [(member (first expr) '(div add sub mult))
     (let-values ([(x new-state) (eval (second expr) state)])
       (if (failure? x)
           (values x state)
           (let-values ([(y new-state2) (eval (third expr) new-state)])
             (if (failure? y)
                 (values y state)
                 (let ((x-val (from-success #f x))
                       (y-val (from-success #f y)))
                   (cond
                     [(equal? (first expr) 'div) 
                      (values (safe-div x-val y-val) new-state2)]
                     [(equal? (first expr) 'add) 
                      (values (success (+ x-val y-val)) new-state2)]
                     [(equal? (first expr) 'sub) 
                      (values (success (- x-val y-val)) new-state2)]
                     [else (values (success (* x-val y-val)) new-state2)]))))))]
    
    [(equal? (first expr) 'define)
 (if (assoc (second expr) state)
     (values (failure (list "Error: variable already defined" (second expr))) state)
     (if (= (length expr) 2)
         ;; Define without value (undefined)
         (values (success (second expr)) (cons (cons (second expr) 'undefined) state))
         ;; Define with expression
         (let-values ([(val new-state) (eval (third expr) state)])
         (if (failure? val)
             (values val state)
             (values (success (second expr)) 
             (update-var (second expr) (from-success #f val) new-state))))))]
    
    [(equal? (first expr) 'assign)
     (if (assoc (second expr) state)
         (let-values ([(val new-state) (eval (third expr) state)])
         (if (failure? val)
             (values val state)
             (values (success (second expr))
                   (update-var (second expr) (from-success #f val) new-state))))
         (values (failure (list "Error: variable not defined" (second expr))) state))]))
    
;; REPL function
(define (repl)
  (let loop ((state (initial-state)))
    (display (format "State: ~a" 
      (map (λ (pair) 
             (list (car pair) 
                   (if (equal? (cdr pair) 'undefined) 'undefined (cdr pair))))
           state)))
    (flush-output)
    (let ((input (read)))
      (when (not (equal? input 'quit))
        (let-values ([(result new-state) (eval input state)])
          (if (success? result)
              (begin
                (displayln (format "Success: ~a" (from-success #f result))))
              (begin
                (displayln (format "Failure: ~a" (from-failure #f result)))))
          (loop new-state))))))


;; Start the REPL
(displayln "Welcome to the expression evaluator. Enter 'quit' to exit.")
(repl)
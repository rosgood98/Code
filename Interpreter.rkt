;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Interpreter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Problem 3

;;; Two example Husky programs:
;;; ((fun (abs) (list (abs (- 5 7))                     ; Should produce
;;;                   (abs (- 7 5))))                   ; '(2 2).
;;;  (fun (x) (if (< x 0) (- x) x)))
;;;
;;; ((fun (x) (if (< x 0) (const (x is negative))       ; Should produce
;;;           (if (= x 0) (const (x is zero))           ; '(x is negative).
;;;               (const (x is positive)))))
;;;  (* 3 (- 7 10)))
;;;
;;; For these two programs to work, you'd have to run them with a top-level
;;; environment that provided bindings for the five variables
;;;     list - < = *

;;; Semantics -- semantic values, environments and the interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A Value is one of:
;;; - SExp              (a number, boolean, cons, etc.)
;;; - Procedure
;;;
;;; A Procedure is one of:
;;; - (make-closure [List-of Var] HExp Env)     (a user procedure)
;;; - (make-primop [[List-of Value] -> Value])  (a built-in procedure)
;;;
;;; Closures are what we get when we evaluate (fun (<var> ...) <body>) terms
;;; in some given environment -- we package up the parameters (<var> ...),
;;; the function's <body> expression, and the current environment into a
;;; closure.
;;;
;;; Primops represent "built-in" primitive operations that the interpreter does
;;; directly. Every primop comes with a handler that we use to do the primop.

(define-struct closure (params body env))
(define-struct primop  (handler))

;;; Env = [Listof (make-binding Var Value)]
(define-struct binding (var val))
;;; An environment represents a set of variable/value bindings.

;;; lookup: Env Var -> Val
;;; Look up the variable's value in the environment.
;;; Environments are scanned left to right, so consing a binding for variable
;;; V onto the front of a list shadows any other bindings of V further down
;;; in the environment.
(define (lookup env var)
  (cond [(empty? env) (error 'lookup "Variable is not bound: " var)]
        [else (local [(define b (first env))]
                (cond [(symbol=? (binding-var b) var)
                       (binding-val b)]
                      [else (lookup (rest env) var)]))]))

(define test-env (list (make-binding 'x 5)
                       (make-binding 'y 2)
                       (make-binding 'x 7)))
(check-expect (lookup test-env 'x) 5)
(check-expect (lookup test-env 'y) 2)
(check-error  (lookup test-env 'z) "lookup: Variable is not bound: 'z")


;;; Eval & Apply -- the yin/yang pair that make the interpreter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Symbol SExp -> Boolean
;;; Is the s-expression the given keyword?
(define (keyword=? kwd sexp)                    ; Cheesy little syntax
  (and (symbol? sexp)                           ; utility function.
       (symbol=? kwd sexp)))

;;; Value [Listof Value] -> Value
;;; Apply the Husky function to the Husky arguments.
(define (app f args)
  (cond [(closure? f)                           ; Proc is a closure:
         (eval (closure-body f)                 ; Eval the function's body
               (append (map make-binding        ; in the closure env extended
                            (closure-params f)  ; with the parameter/argument
                            args)               ; bindings.
                       (closure-env f)))]

        ;; Just hand off to primop's handler.
        [(primop? f) ((primop-handler f) args)]

        [else (error 'app "Attempting to apply a non-function: " f)]))

(check-error (app 5 (list 5 4 3)) "app: Attempting to apply a non-function: 5")

;; HExp -> Value
;; Run the Husky expression in the top-level environment
(define (run e) (eval e top-env))

(check-expect (run '1)         1)
(check-expect (run '(plus2 5)) 7)
(check-expect (run '(plus2 5)) 7)
(check-expect (run '((fun (x) (minus1 (plus2 x))) 5)) 6)
(check-expect (run '((fun (f) (f (f 0))) plus2)) 4)

;;; Primops & the top-level environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Convert a Racket/ISL/ASL function into a Husky-interpreter primop.
(define (racket->husky-primop f)
  (make-primop (lambda (args) (apply f args))))

;;; More examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-expect (run '((fun (abs) (list (abs (- 5 7))
                                      (abs (- 7 5))))
                     (fun (x) (if (< x 0) (- x) x))))
              '(2 2))

(check-expect (run '((fun (x) (if (< x 0) (const (x is negative))
                                  (if (= x 0) (const (x is zero))
                                      (const (x is positive)))))
                     (* 3 (- 7 10))))

              '(x is negative))

;;; The hard way to write 120 -- really exercise the interpreter.
;;; Apply the Y combinator to a factorial generator; apply the result to 5.
;;; Yahoo.
;;;

(check-expect (run '(((fun (f) ((fun (g) (f (fun (x) ((g g) x))))
                                (fun (g) (f (fun (x) ((g g) x))))))
                      (fun (fact)
                           (fun (n) (if (= n 0) 1
                                        (* n (fact (- n 1)))))))
                     5))
              120)

; Part 1

;;; A HExp (Husky Expression) is one of:
;;;   The Data                                  ; How it looks as an s-exp
;;;   --------                                  ; ------------------------
;;; - Number                                    ; <number>
;;; - Var                                       ; <var>
;;; - (list 'const SExp)                        ; (const <constant-sexpression>)
;;; - (list 'fun (list Var ...) HExp)           ; (fun (<var> ...) <body>)
;;; - (list 'if HExp HExp HExp)                 ; (if <test> <then> <else>)
;;; - (list HExp HExp ...)                      ; (<fun> <arg> <arg> ...)
;;; - (list 'and HExp HExp ...)                 ; (and <expr> <expr> ...)
;;; - (list 'or HExp HExp ...)                  ; (or <expr> <expr> ...)
;;;
;;; A Var is a Symbol.

;; eval : HExp Env -> Value
;; Evaluate the Husky expression in the given environment.

(define (eval exp env)
  (cond [(number? exp) exp]    
        [(symbol? exp) (lookup env exp)]
        [(string? exp) exp]
        [(cons? exp)
         (local [(define e1 (first exp))]
           (cond
             [(keyword=? 'const e1) (second exp)]
             [(keyword=? 'fun e1)
              (make-closure (second exp) 
                            (third exp)  
                            env)]        
             [(keyword=? 'if e1)
              (eval ((cond [(eval (second exp) env) third]
                           [else                    fourth]) exp)
                    env)]
             [(keyword=? 'or e1)
              (ormap (lambda (x) (eval x env)) (rest exp))]
             [(keyword=? 'and e1)
              (andmap (lambda (x) (eval x env)) (rest exp))]
             [(keyword=? 'let e1)
              (eval (third exp) (eval (second exp) env))] 
             [else (app (eval (first exp) env)
                        (map (lambda (a) (eval a env))
                             (rest exp)))]))]
        [else (error 'eval "Not a valid Husky expression: " exp)]))

(check-expect (eval '(or (= 2 2) (= 2 1)) top-env) #t)
(check-expect (eval '(or (= 2 3) (= 2 1)) top-env) #f)
(check-expect (eval '(and (= 2 2) (= 4 4)) top-env) #t)
(check-expect (eval '(and (= 2 2) (= 4 3)) top-env) #f)
(check-expect (eval "hello" top-env) "hello")
(check-error (eval = top-env) "eval: Not a valid Husky expression: =")

; Part 2

(define top-env
  (list (make-binding 'plus1  (racket->husky-primop add1))
        (make-binding 'minus1 (racket->husky-primop sub1))
        (make-binding 'head   (racket->husky-primop first))
        (make-binding 'tail   (racket->husky-primop rest))
        (make-binding 'list   (racket->husky-primop list))
        (make-binding 'tru    true)
        (make-binding 'fals   false)
        (make-binding 'not    (racket->husky-primop not))
        (make-binding 'plus2  (racket->husky-primop (lambda (n) (+ n 2))))
        (make-binding '*      (racket->husky-primop *))
        (make-binding '+      (racket->husky-primop +))
        (make-binding '-      (racket->husky-primop -))
        (make-binding '=      (racket->husky-primop =))
        (make-binding '<      (racket->husky-primop <))
        (make-binding 'string-append (racket->husky-primop string-append))
        (make-binding 'substring (racket->husky-primop substring))))

;; Author: Christopher Olsen
;; Copyright 2013
;; License: GNU GPLv3 (ask if you'd like it licensed differently)
;;
;; Fez is a simple Scheme REPL built in Clojure based mostly on SICP's Chapter 
;;  4 which should be considered the main source of documentation.  It isn't 
;;  a full Scheme, it's more like Clojure's wearing a fun hat - like a fez.
;;
;; SICP = The Structure and Interpretation of Computer Programs by Hal Abelson
;;        and Gerald Sussman 
;;        http://mitpress.mit.edu/sicp/full-text/book/book.html

(ns fez.core)

(defn about-fez
  []
  (println "\n\nWelcome to Fez!  A tiny Scheme-like LISP implemented in Clojure by Christopher Olsen.  For implementation details see the included source code and Chapter 4 of The Structure and Interpretation of Computer Programs (SICP).\n\nFez is just for fun, it's not meant for production code but for exploration of different LISP's, how they're similar, how they're different...\n\nThe name!  Why Fez?  Well Clojure already had the JVM shoes, the Java pants and the Clojure jacket, but something was missing.  So now it has a Scheme-like hat.  I quietly hope to get Fez to the point that somebody can implement Python in it and call it 'Tassle'.\n"))

;; since the defn's aren't in order, we're just going to forward declare 
;; *everything* except the driver loop (I really want to keep this in at least
;; roughly the order it appears in the book, and this is just for fun, so...)
(declare scm-eval scm-apply eval-if eval-sequence eval-assignment eval-definition self-evaluating? variable? quoted? text-of-quotation tagged-list? assignment? assignment-variable assignment-value definition? definition-variable definition-value lambda? lambda-parameters lambda-body make-lambda if? if-predicate if-consequent if-alternative make-if begin? begin-actions last-exp? first-exp rest-exps sequence->exp make-begin application? operator operands no-operands? first-operand rest-operands cond? cond-clauses cond-else-clause? cond-predicate cond-actions cond->if expand-clauses copy-environment make-procedure compound-procedure? procedure-parameters procedure-body procedure-environment enclosing-environment first-frame the-empty-environment make-frame frame-variables frame-values add-binding-to-frame! extend-environment lookup-variable-value set-variable-value! define-variable! setup-environment the-global-environment primitive-procedure? primitive-implementation primitive-procedures primitive-procedure-names primitive-procedure-objects apply-primitive-procedure about-fez error)

(def ^:dynamic *debug* false)

(defn scm-eval
  [exp env]
  ;;(println "\nscm-eval with:" exp "\nand env:" env "\n")
  (cond (self-evaluating? exp) exp
        (variable? exp) (lookup-variable-value exp env)
        (quoted? exp) (text-of-quotation exp)
        (assignment? exp) (eval-assignment exp env)
        (definition? exp) (eval-definition exp env)
        (if? exp) (eval-if exp env)
        (lambda? exp) (make-procedure (lambda-parameters exp)
                                      (lambda-body exp)
                                      env)
        (begin? exp) (eval-sequence (begin-actions exp) env)
        (cond? exp) (scm-eval (cond->if exp) env)
        (application? exp) (scm-apply (scm-eval (operator exp) env)
                                      (map #(scm-eval % env) (operands exp)))
                                        ;(list-of-values (operands exp) env))
        :else (error "Unknown expression type -- EVAL " exp)))

(defn scm-apply
  [procedure arguments]
  (if *debug* (println "scm-apply, procudure:" procedure "\n  arguments:" arguments))
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure
                                                                    arguments)
        (compound-procedure? procedure) (eval-sequence
                                         (procedure-body procedure)
                                         (extend-environment
                                          (procedure-parameters procedure)
                                          arguments
                                          (procedure-environment procedure)))
        :else (error ("Unknown procedure type -- APPLY " procedure))))

(defn eval-if
  "conditionals"
  [exp env]
  (if (true? (scm-eval (if-predicate exp) env))
    (scm-eval (if-consequent exp) env)
    (scm-eval (if-alternative exp) env)))

(defn eval-sequence
  "sequences"
  [exps env]
  (cond (last-exp? exps) (scm-eval (first-exp exps) env)
        ;; this cons not in SICP but seems necessary
        :else (cons (scm-eval (first-exp exps) env)
                    (eval-sequence (rest-exps exps) env))))

(defn eval-assignment
  [exp env]
  (set-variable-value! (assignment-variable exp)
                       (scm-eval (assignment-value exp) env)
                       env)
  'ok)

(defn eval-definition
  [exp env]
  (define-variable! (definition-variable exp)
    (scm-eval (definition-value exp) env)
    env)
  'ok)

;; representing expressions

(defn self-evaluating?
  "numbers and strings"
  [exp]
  (cond (number? exp) true
        (string? exp) true
        :else false))

(defn variable?
  [exp]
  (symbol? exp))

(defn quoted? 
  [exp]
  (tagged-list? exp 'quote))

(defn text-of-quotation
  [exp]
  (nth exp 1))

(defn tagged-list?
  [exp tag]
  (if (list? exp)
    (= (first exp) tag)
    false))

(defn assignment?
  [exp]
  (tagged-list? exp 'set!))

(defn assignment-variable
  [exp]
  (nth exp 1))

(defn assignment-value
  [exp]
  (nth exp 2))

(defn definition?
  [exp]
  (tagged-list? exp 'define))

(defn definition-variable 
  [exp]
  (if (symbol? (nth exp 1))
    (nth exp 1)
    (first (first (rest exp)))))

(defn definition-value
  [exp]
  (if (symbol? (nth exp 1))
    (first (rest (rest exp)))
    (make-lambda (rest (first (rest exp)))   ; formal parameters
                 (rest (rest exp)))))        ; body

(defn lambda?
  [exp]
  (tagged-list? exp 'lambda))

(defn lambda-parameters
  [exp]
  (if *debug* (println "in lambda-parameters with:" exp))
  (first (rest exp)))

(defn lambda-body
  [exp]
  (if *debug* (println "in lambda-body with:" exp))
  (rest (rest exp)))

(defn make-lambda
  [parameters body]
  (cons 'lambda (cons parameters body)))

(defn if?
  [exp]
  (tagged-list? exp 'if))

(defn if-predicate 
  [exp]
  (first (rest exp)))

(defn if-consequent
  [exp]
  (first (rest (rest exp))))

(defn if-alternative
  [exp]
  (if (not (nil? (rest (rest (rest exp)))))
    (first (rest (rest (rest exp))))
    'false))

(defn make-if
  "used by cond->if"
  [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn begin?
  [exp]
  (tagged-list? exp 'begin))

(defn begin-actions
  [exp]
  (rest exp))

(defn last-exp?
  [sequence]
  (= 1 (count sequence)))

(defn first-exp
  [sequence]
  (first sequence))

(defn rest-exps
  [sequence]
  (rest sequence))

(defn sequence->exp
  "used by cond->if, transforms a sequence into a single expression "
  [seq]
  (cond (nil? seq) seq
        (last-exp? seq) (first-exp seq)
        :else (make-begin seq)))

(defn make-begin
  [seq]
  (conj 'begin seq))

(defn application?
  [exp]
  (list? exp)) ;; is "list?" a good approximation of "pair?"?

(defn operator
  [exp]
  (first exp))

(defn operands
  [exp]
  (rest exp))

(defn no-operands?
  [ops]
  (empty? ops))

(defn first-operand
  [ops]
  (nil? ops)) ;;empty? ?

(defn rest-operands
  [ops]
  (rest ops))


;; derived expressions

(defn cond?
  [exp]
  (tagged-list? exp 'cond))

(defn cond-clauses
  [exp]
  (rest exp))

(defn cond-else-clause?
  [clause]
  (= (cond-predicate clause) 'else))

(defn cond-predicate
  [clause]
  (first clause))

(defn cond-actions
  [clause]
  (rest clause))

(defn cond->if
  [exp]
  (expand-clauses (cond-clauses exp)))

(defn expand-clauses
  [clauses]
  (if (nil? clauses)
    'false ;; no else clause
    (let [_first (first clauses) ;; first and rest name-mangled to avoid clashes
          _rest (rest clauses)]
      (if (cond-else-clause? _first)
        (if (nil? _rest)
          (sequence->exp (cond-actions _first))
          (error "ELSE clause isn't last -- COND->IF"
                 clauses))
        (make-if (cond-predicate _first)
                 (sequence->exp (cond-actions _first))
                 (expand-clauses _rest))))))

;; predicates

;; (defn true?
;;   [x]
;;   (not (= x false)))
;; (defn false?
;;   [x]
;;   (= x false))

;; procedures

(defn copy-environment
  "Copies an environment into a new atom to avoid infinite recursion of lambda
   functions being defined in the environments they're defined in.  This has 
   implications for how lambda functions behave."
  [environment]
  (doall (map #(atom (deref %)) environment))) 

(defn make-procedure
  [parameters body env]
  (doall (list 'procedure parameters body (copy-environment env))))

(defn compound-procedure?
  [p]
  (tagged-list? p 'procedure))

(defn procedure-parameters
  [p]
  (first (rest p)))

(defn procedure-body
  [p]
  (first (rest (rest p))))

(defn procedure-environment
  [p]
  (first (rest (rest (rest p)))))


;; operations on environments (not production speed implementation)

;; what follows is the biggest break from the SICP book as far as implementation
;; goes.  For scheme in scheme they represented frames as lists of the form
;; ((var1 var2 var3) val1 val2 val3), but for scheme in clojure I've chosen to
;; represent frames as maps {var1 val1 var2 val2}

(defn enclosing-environment
  [env]
  (rest env))

(defn first-frame
  [env]
  (first env))

(def the-empty-environment
  ()) ;; vector since empty list is a problem?

(defn make-frame
  [variables values]
  (atom (zipmap variables values)))

(defn frame-variables
  [frame]
  (keys @frame))

(defn frame-values
  [frame]
  (vals @frame))

(defn add-binding-to-frame!
  [var val frame]
  (swap! frame #(assoc % var val)))

(defn extend-environment
  [vars vals base-env]
  (if (= (count vars) (count vals))
    (cons (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(defn lookup-variable-value
  [var env]
  (if *debug* (println "in lookup-variable-value with:" var))
  (cond (= the-empty-environment env) (error "Unbound variable" var)
        (contains? (deref (first env)) var) ((deref (first env)) var)
        :else (lookup-variable-value var (rest env))))

(defn set-variable-value!
  "Searches for the variable in the local to global frames and updates it where
   it's found.  Returns error if it isn't found"
  [var val env]
  (cond (= the-empty-environment env) (error "Unbound variable" var)
        (contains? (deref (first env)) var)
        (swap! (first env) #(assoc % var val))
        :else (set-variable-value! var val (rest env))))

(defn define-variable!
  [var val env]
  (if *debug* (println "\ninside define-variable, var:" var "\nval:" val "\nenv:" env))
  (let [frame (first-frame env)]
    (if *debug* (println "\ninside define-variable!'s let loop\nframe:" frame "\nderef'd frame:" (deref frame) "\nvar:" var "\nval:" val))
    (swap! frame #(assoc % var val))))


;; running the evaluator as a program

(defn square
  [x]
  (* x x))

(def primitive-procedures ;; should this be a map?  probably....
  [(list 'car first) 
   (list 'cdr rest)
   (list 'cons cons)
   (list 'null nil)   ;; will this work???
   (list '+ +)
   (list '- -)
   (list '* *)
   (list '/ /)
   (list 'about-fez about-fez)
   (list 'sq square)
   (list 'cube (fn [x] (* x x x)))
   ;;<more primitives>
   ])

(def primitive-procedure-names
  (map first primitive-procedures))

(def primitive-procedure-objects
  (map (fn [proc] (list 'primitive (first (rest proc)))) primitive-procedures))

(defn apply-in-underlying-clojure
  [proc args]
  (apply proc args))

(defn apply-primitive-procedure 
  [proc args]
  (apply-in-underlying-clojure
   (primitive-implementation proc) args))

(defn setup-environment
  []
  (let [initial-env (extend-environment primitive-procedure-names
                                        primitive-procedure-objects
                                        the-empty-environment)]
    ;; (define-variable! 'true true initial-env)
    ;; (define-variable! 'false false initial-env)
    initial-env))

(def the-global-environment
  (setup-environment))

(defn primitive-procedure?
  [proc]
  (tagged-list? proc 'primitive))

(defn primitive-implementation
  [proc]
  (first (rest proc)))


;; driver loop

(def input-prompt ";;; Fez-input: ")
(def output-prompt ";;; Fez-value: ")

(declare prompt-for-input announce-output user-print)
(defn driver-loop
  []
  (doall (prompt-for-input input-prompt))
  (let [input (read-line)]
    (if (= (count input) 0)
      (announce-output output-prompt)
      (let [output (scm-eval (read-string input) the-global-environment)]
        (announce-output output-prompt)
        (user-print output)
        (println))))
  (driver-loop))

(defn prompt-for-input
  [string]
  (println string))

(defn announce-output
  [string]
  (println string))

(defn user-print
  [object]
  (if *debug* (println "in user-print"))
  (if (compound-procedure? object)
    (print (list 'compound-procedure
                 (procedure-parameters object)
                 (procedure-body object)
                 '<prodedure-env>))
    (print object)))

;; custom error macro (calls down to Java's Throwable.  doesn't work very well)
(defmacro error
  [& args]
  `(throw (Throwable. (str ~@args))))


;; main

(defn -main
  [& args]
  (println "\nWelcome to Fez!  A (very) simple Scheme-like LISP implemented in Clojure.\nFor implementation details see the included source code and Chapter 4 of \nThe Structure and Interpretation of Computer Programs (SICP), available \nat http://mitpress.mit.edu/sicp/full-text/book/book.html.  \nType (about-fez) for more details.\n\n") 
  (driver-loop))

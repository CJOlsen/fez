;; Author: Gerald Sussman?
;; License: See video linked below, believed to be Creative Commons but just because the video is doesn't mean a transcription of the video is, too

;; TO BE CLEAR, THIS FILE IS NOT MY WORK I ONLY TRANSCRIBED IT!

;; this is what was written on the board during Lecture 7 of the SICP course, 
;; available at:
;; http://www.youtube.com/watch?v=0m6hoOelZH8
;; I highly recommend watching this lecture....you'll also find out where I got 
;; the idea to call this project Fez!

(define eval
  (lambda (exp env)
    (cond ((number? exp) exp)
	  ((symbol? exp) (lookup exp env))
	  ((eq? (car exp) 'quote) (cadr exp))
	  ((eq? (car exp) 'lambda)
	   (list 'closure (cdr exp) env))
	  ((eq? (car exp) 'cond)
	   (evcond (cdr exp) env))
	  (else (apply (eval (car exp) env)
		       (evlist (cdr exp) env))))))

(define apply
  (lambda (proc args)
    (cond ((primitive? proc) (apply-primop proc args))
	  ((eq? (car proc) 'closure)
	   (eval (cadadr proc) ;; this is where the closure magic happens
		 (bind (caadr proc)
		       args
		       (caddr proc))))
	  (else error)))) 

(define evlist
  (lambda (l env)
    (cond ((eq? l '()) '())
	  (else
	   (cons (eval (car l) env)
		 (evlist (cdr l) env))))))

(define evcond
  (lambda (clauses env)
    (cond ((eq? clauses '()) '())
	  ((eq? (caar clauses) 'else)
	   (eval (cadar clauses) env))
	  ((false? (eval (caar clauses) env))
	   (evcond (cdr clauses) env))
	  (else (eval (cadar clauses))))))

(define bind
  (lambda (vars vals env)
    (cons (pair-up vars vals)
	  env)))

(define pair-up
  (lambda (vars vals)
    (cond
     ((eq? vars '())
      (cond ((eq? vars '())
	     (cond ((eq? vals '()) '())
		   (else (error TMA)))) ;;too many arguments
	    ((eq? vals '()) (error TFA)) ;; too few arguments
	    (else
	     (cons (cons (car vars)
			 (car vals))
		   (pair-up (cdr vars)
			    (cdr vals)))))))))

(define lookup
  (lambda (sym env)
    (cond ((eq? env '()) (error ubv)) ;; empty env?  unbound variable
	  (else
	   ((lambda (vcell) ;; value cell
	      (cond ((eq? vcell '())
		     (lookup sym (cdr env)))
		    (else (cdr vcell))))
	    (assq sym (car env)))))))

(define assq
  (lambda (sym alist)
    (cond ((eq? alist '()) '())
	  ((eq? sym (caar alist))
	   (car alist))
	  (else
	   (assq sym (cdr alist))))))

;; primitive? and prim-op left undefined 

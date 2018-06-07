;; Exercises for Chapter 3

;; 3.1
;; Show a 1 ambda expression that is equivalent to the above 1 et* expression. You may need more than one 1 ambda.
((lambda (x)
   (+ x  ((lambda (y)
	    (* y y))
	  x)))
 6)

;; 3.2
;; The function cons can be seen as a special case of one of the other functions listed previously. W hich one?

;; 3.3
;; Write a function that will print an expression in dotted pair notation. Use the built-in function pri nc to print each component of the expression.

;; 3.4
;; Write a function that, like the regular print function, will print an expression in dotted pair notation when necessary but will use normal list notation when possible.



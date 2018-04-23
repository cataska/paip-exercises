;; Exercises for Chapter 3

;; 3.1
;; Show a 1 ambda expression that is equivalent to the above 1 et* expression. You may need more than one 1 ambda.
((lambda (x)
   (+ x  ((lambda (y)
	    (* y y))
	  x)))
 6)

;; 3.2


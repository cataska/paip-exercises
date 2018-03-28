;; Exercises for Chapter 1

;; 1.1
;; Define a version of last-name that handles "Rex Morgan MD,"
;; "Morton Downey, Jr.," and whatever other cases you can think of.
(defparameter *suffixes*
  '(MD Jr))

(defun last-name (name)
  (if (member (first (last name)) *suffixes*)
      (last-name (butlast name))
      (first (last name))))

;; 1.2
;; Write a function to exponentiate, or raise a number to an integer
;; power. For example: (power 3 2) = 3^ = 9.
(defun power (x n)
  (if (<= n 0)
      1
      (* x (power x (1- n)))))

;; 1.3
;; Write a function that counts the number of atoms in an expression.
;; For example: (count-atoms '(a (b) c)) = 3.
;; Notice that there is something of an ambiguity in this: should (a nil c) count as three atoms, or as two, because it is equivalent to (a () c)?
(defun count-atoms (lst)
  (if (null lst)
      0
      (if (not (consp (first lst)))
	  (1+ (count-atoms (rest lst)))
	  (+ (count-atoms (first lst)) (count-atoms (rest lst))))))

;; 1.4
;; Write a function that counts the number of times an expression occurs anywhere within another expression. Example: (count-anywhere 'a '(a ((a) b) a)) 3.
(defun count-anywhere (sym lst)
  (if (null lst)
      0
      (if (consp (first lst))
	  (+ (count-anywhere sym (first lst))
	     (count-anywhere sym (rest lst)))
	  (if (eql sym (first lst))
	      (1+ (count-anywhere sym (rest lst)))
	      (count-anywhere sym (rest lst))))))

;; 1.5
;; Write a function to compute the dot product of two sequences of numbers, represented as lists. The dot product is computed by multiplying corresponding elements and then adding up the resulting products. Example:
;; (dot-product '(10 20) '(3 4)) = 10 * 3 + 20 * 4 = 110
(defun dot-product (x y)
  (+ (* (first x)
	(first y))
     (* (first (rest x))
	(first (rest y)))))

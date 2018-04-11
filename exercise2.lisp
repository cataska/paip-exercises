;; Exercises for Chapter 2

(defun sentence () (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article () (one-of '(the a)))
(defun Noun () (one-of '(man ball woman table)))
(defun Verb () (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category"
  (rule-rhs (assoc category *grammar*)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is
  *simple-grammar*, but we can switch to other grammars.")

;; 2.1
;; Write a version of generate that uses cond but avoids calling
;; rewrites twice.
(defun generate (phrase)
  (let ((choices nil))
    (cond ((listp phrase)
	   (mappend #'generate phrase))
	  ((setf choices (rewrites phrase))
	   (generate (random-elt choices)))
	  (t (list phrase)))))

;; 2.2
;; Write a version of generate that explicitly differentiates between
;; terminal symbols (those with no rewrite rules) and nonterminal symbols.
(defun non-terminal? (cat)
  (not (null (rewrites cat))))

(defun generate (phrase)
  (condp ((listp phrase)
	  (mappend #'generate phrase))
	 ((non-terminal? phrase)
	  (generate (random-elt (rewrites phrase))))
	 (t (list phrase))))

;; 2.3
;; Write a trivial grammar for some other language. This can be a
;; natural language other than English, or perhaps a subset of a computer language.
(defparameter *x86assembly-grammar*
  '((instruction -> (operator operand*))
    (operator -> MOV ADD LEA)
    (operand* -> (operand operand))
    (operand -> %EAX %EBX %ECX %EDX)))

;; 2.4
;; One way to describing combine-all is that it calculates the cross-
;; product of the functions append on the argument lists. Write the higher-order function
;; cross-product, and define combine-all in terms of it.
;; The moral is the make your code as general as possible, because you never know what
;; you may want to do with it next.
(defun cross-product (fn xlist ylist)
  (mappend #'(lambda (y)
	       (mapcar #'(lambda (x) (funcall fn x y))
		       xlist))
	   ylist))

(defun combine-all (xlist ylist)
  (cross-product #'append xlist ylist))


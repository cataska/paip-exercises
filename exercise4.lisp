;; Exercises for Chapter 4

;; 4.1
;; It is possible to implement dbg using a single call to format. Can
;; you figure out the format directives to do this?
(defun new-dbg (id format-string &rest args)
  (when (member id *dbg-ids*)
    (let ((new-format-string (concatenate 'string "~&" format-string)))
      (format *debug-io* new-format-string args))))

;; 4.2
;; Write a function that generates all permutations of its input.
(defun permutations (xs)
  (if (endp (cdr xs))
      (list xs)
      (loop for x in xs
	 append (loop for ys in (permutations (remove x xs
						      :count 1
						      :test #'eql))
		   collect (cons x ys)))))


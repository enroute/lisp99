;; p01 find the last box of a list
;; example:
;; (my-last '(a b c d)) => (d)
(defun my-last (LIST)
  (nthcdr (1- (length LIST)) LIST))

;; p02 find the last but one box of a list
;; example:
;; (my-but-last '(a b c d)) => (c d)
(defun my-but-last (LIST)
  (nthcdr (- (length LIST) 2) LIST))

;; p03 find the k'th element of a list
;; the first element in the list is number 1.
;; example:
;; (element-at '(a b c d e) 3) => c
(defun element-at (LIST index)
  (nth (1- index) LIST))

;; p04 find the number of elements of a list
(length '(1 2 3 4 5))

;; p05 reverse a list
(reverse '(a b c d e))

;; p06 find out whether a list is a palindrome
;; a palindrome can be read forward or backward.
;; e.g. (palindrome '(x a m a x)) => t
(defun palindrome (LIST)
  (equal LIST (reverse LIST)))

;; p07 flatten a nested list structure
;; transform a list, possibly holding lists as as elements into
;; a `flat' list by replacing each list with its elements(recursively).
;; example:
;; (my-flatten '(a (b (c d) e))) => (a b c d e)
;; hints: use the predefined functions list and append
(defun my-flatten (LIST)
  (if (equal LIST nil)
      nil
    (let ((elem (car LIST))
	  (rest-list (cdr LIST)))
      (if (listp elem)
	  (append (my-flatten elem) (my-flatten rest-list))
	(append (cons elem nil) (my-flatten rest-list))))))

;; p08 eliminate consecutive duplicates of list elements
;; if a list contains repeated elements they should be replaced with
;; a single copy of the element. The order of the elements should not
;; be changed.
;; example:
;; (my-compress '(a a a a b c c a a d e e e e )) => (a b c a d e)
(defun my-compress (LIST)
  (let* ((last-elem (car LIST))
	 (compress-list (list last-elem)))
    (dolist (elem (cdr LIST))
      (unless (equal elem last-elem)
	(setq compress-list (append compress-list (cons elem nil)))
	(setq last-elem elem)))
    compress-list))

;; p09 pack consecutive duplicates of list elements into sublists
;; if a list contains repeated elements they should be placed in
;; sepaarate sublists.
;; example:
;; (my-pack '(a a a a b c c a a d e e e e))
;; =>((a a a a) (b) (c c) (a a) (d) (e e e e))
(defun my-pack (LIST)
  (let ((sublist (list (car LIST)))
	(packlist nil))
    (dolist (elem (cdr LIST))
      (if (equal elem (car sublist))
	  (push elem sublist)
	(progn
	  (setq packlist (append packlist (cons sublist nil)))
	  (setq sublist (list elem)))))
    (setq packlist (append packlist (cons sublist nil)))
    packlist))

;; P10 run-length encoding of a list
;; Use the result of problem P09 to implement the so-called run-length
;; encoding data compression method. Consecutive duplicates of
;; elements are encoded as lists (N E) where N is the number of
;; duplicates of the element E.
;; example:
;; (my-run-length-use-pack '(a a a a b c c a a d e e e e))
;; =>((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))
(defun my-run-length-use-pack (LIST)
  "version using `my-pack'."
  (let ((packlist (my-pack LIST))
	(encodelist nil))
    (dolist (sublist packlist)
      (setq encodelist 
	    (append encodelist 
		    (cons (list (length sublist) (car sublist)) nil))))
    encodelist))

;; (my-run-length '(a a a a b c c a a d e e e e))
;; =>((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))
(defun my-run-length (LIST)
  "version w/o using `my-pack'."
  (let ((lastelem (car LIST))
	(packlist nil)
	(count 1))
    (dolist (elem (cdr LIST))
      (if (equal elem lastelem)
	  (setq count (1+ count))
	(progn
	  (setq packlist (append packlist (cons (list count lastelem) nil)))
	  (setq count 1)
	  (setq lastelem elem))))
    (setq packlist (append packlist (cons (list count lastelem) nil)))))

;; P11 Modified run-length encoding.
;; Modify the result of problem P10 in such a way that if an element
;; has no duplicates it is simply copied into the result list. Only
;; elements with duplicates are transferred as (N E) lists.
;; Example:
;; (my-run-length-modify '(a a a a b c c a a d e e e e))
;; =>((4 a) b (2 c) (2 a) d (4 e))
(defun my-run-length-modify (LIST)
  (let ((lastelem (car LIST))
	(packlist nil)
	(count 1))
    (dolist (elem (cdr LIST))
      (if (equal elem lastelem)
	  (setq count (1+ count))
	(progn
	  (setq packlist (append packlist ; judegment
				 (if (= 1 count)
				     (cons lastelem nil)
				   (cons (list count lastelem) nil))))
	  (setq count 1)
	  (setq lastelem elem))))
    (setq packlist (append packlist	; judement
			   (if (= 1 count)
			       (cons lastelem nil)
			     (cons (list count lastelem) nil))))))


;; P12 Decode a run-length encoded list.
;; Given a run-length code list generated as specified in problem
;; P11. Construct its uncompressed version.
;; (my-unpack-run-length '((4 a) b (2 c) (2 a) d (4 e)))
;; =>(a a a a b c c a a d e e ...)
(defun my-unpack-run-length (LIST)
  (let (unpacklist)
    (dolist (elem LIST)
      (if (listp elem)
	  (setq unpacklist (append unpacklist 
				   (make-list (car elem) (nth 1 elem))))
	(setq unpacklist (append unpacklist (cons elem nil)))))
    unpacklist))

;; P13 Run-length encoding of a list (direct solution).
;; Implement the so-called run-length encoding data compression method
;; directly. I.e. don't explicitly create the sublists containing the
;; duplicates, as in problem P09, but only count them. As in problem
;; P11, simplify the result list by replacing the singleton lists (1
;; X) by X.
;; 
;; Already done in P11.

;; P14 Duplicate the elements of a list.
;; Example:
;; (my-duplicate '(a b c c d))
;; =>(a a b b c c c c d d)
(defun my-duplicate (LIST)
  (let (duplist)
    (dolist (elem LIST)
      (setq duplist (append duplist (list elem elem))))
    duplist))

P15 (**) Replicate the elements of a list a given number of times.
Example:
;; (my-replicate '(a b c) 3)
;; =>(a a a b b b c c c)
(defun my-replicate (LIST repeat)
  (let (replist)
    (dolist (elem LIST)
      (setq replist (append replist (make-list repeat elem))))
    replist))
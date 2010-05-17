;; p1. Fibonacci sequence.
;; F(1)=F(2)=1
;; F(n)=F(n-1)+F(n-2), for n>2.
;; example:
;; (my-fibonacci 8) => 21
(defun my-fibonacci-recursive (n)
  (if (< n 3)
      1
    (+ (my-fibonacci (1- n)) (my-fibonacci (- n 2)))))
(defun my-fibonacci (n)
  (if (< n 3)
      1
    (let ((fk-1 1)
	  (fk-2 1)
	  fk
	  (k-1 2))
      (while (< k-1 n)
	(setq fk (+ fk-1 fk-2))
	(setq fk-2 fk-1)
	(setq fk-1 fk)
	(setq k-1 (1+ k-1)))
      fk)))

(my-fibonacci 1000)

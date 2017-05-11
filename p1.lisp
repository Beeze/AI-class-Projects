(defun char-to-string (character)
  (case character
    (#\A (string "A"))
    (#\T (string "T"))
    (#\G (string "G"))
    (#\C (string "C"))))

(defun num-to-string (num)
  (string num))

(defun complement-base (base)
  (case base
    (#\A (string "T"))
    (#\T (string "A"))
    (#\G (string "C"))
    (#\C (string "G"))))

(defvar *base* #\C)
(print (complement-base *base*))

(defun complement-strand (strand)
(let ((complement (make-array 1 :adjustable t :fill-pointer 0)))
  (loop for c across strand do
    (vector-push-extend (complement-base c) complement))
  (print complement)))

(defvar *strand* "AGTC")
(complement-strand *strand*)

(defun make-double (strand)
(let ((double-strand (make-array 1 :adjustable t :fill-pointer 0)))
  (loop for c across strand do
    (vector-push-extend (concatenate 'string (char-to-string c) (complement-base c)) double-strand))
    (print double-strand)))

(make-double *strand*)

(defun count-bases (strand)
(setq numA 0)
(setq numC 0)
(setq numG 0)
(setq numT 0)
(loop for c across strand do
  (case c
    (#\A (setq numA (+ numA 1)))
    (#\C (setq numC (+ numC 1)))
    (#\G (setq numG (+ numG 1)))
    (#\T (setq numT (+ numT 1)))))

(print
  (concatenate
    'string "A" (write-to-string numA)
    " C" (write-to-string numC)
    " G" (write-to-string numG)
    " T" (write-to-string numT))))

(count-bases *strand*)

(defun prefixp (strand prefix)
  (if (> (length prefix) (length strand))
    (print nil)
    (progn
      (setq len (length prefix))
      (setq beginning-of-string (subseq strand 0 len))
      (if (string= beginning-of-string prefix)
        (print t)
        (print nil)))))

(prefixp "AGCT" "AGC")

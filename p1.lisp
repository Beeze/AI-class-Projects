(defun complement-base (base)
  (case base
    ('A (quote T))
    ('T (quote A))
    ('G (quote C))
    ('C (quote G))))

(print (complement-base 'A))


(defun complement-strand (strand)
  (setq complement-strand '())
  (loop for c in strand do
    (setq complement-strand (append  complement-strand (list (complement-base c)))))
  (print complement-strand))

(defvar *strand* '(A G T C))
(complement-strand *strand*)


(defun make-double (strand)
  (setq double-strand '())
    (loop for c in strand do
      (setq double-strand (append double-strand (list (list c (complement-base c))))))
  (print double-strand))

(make-double *strand*)


(defun count-bases (strand)
  (setq numA 0)
  (setq numC 0)
  (setq numG 0)
  (setq numT 0)
  (loop for c in strand do
    (case c
      ('A (setq numA (+ numA 1)))
      ('C (setq numC (+ numC 1)))
      ('G (setq numG (+ numG 1)))
      ('T (setq numT (+ numT 1)))))
  (print (list
    (list 'A numA)
    (list 'C numC)
    (list 'G numG)
    (list 'T numT))))

(count-bases *strand*)

;refactor to take in a list and return a list
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

;refactor to take in a list and return a list
(defun appearsp (strand check)
  (if (> (length check) (length strand))
    (print nil)
    (progn
      (setq it 0)
      (loop for c in strand do
        (if (>= it (+ (- (length strand) (length check)) 1))
          (progn
            (print nil)
            (return))
          (progn
            (setq snippet (subseq strand it (+ it (length check))))
            (if (equal snippet check)
              (progn
                (print t)
                (return)))))
        (setq it (+ it 1))))))

(appearsp '(A F G C T) '(G C))

(defun prefix (num strand)
  (if (> num (length strand))
    (print "Please enter a number that is less than the length of the strand")
    (print (subseq strand 0 num))))

(prefix 4 '(A F G C T F))


;(defun draw-dna (strand)
;  (dotimes (length strand)
;    (print '-))
;  (dotimes (length strand)
;    (print '!))
;  (loop for c in strand do
;    (print c))
;  (dotimes (length strand)
;    (print ':))
;  (loop for c in strand do
;    (print (complement-base c)))
;  (dotimes (length strand)
;    (print '!))
;  (dotimes (* (length strand) 6)
;    (print '-)))

;(draw-dna '(A G T))

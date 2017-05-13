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


(defun prefixp (strand prefix)
  (if (> (length prefix) (length strand))
    (print nil)
    (progn
      (setq len (length prefix))
      (setq beginning-of-string (subseq strand 0 len))
      (if (equal beginning-of-string prefix)
        (print t)
        (print nil)))))

(prefixp '(A G C T) '(A G C))


(defun appearsp (strand check)
  (if (> (length check) (length strand))
    (return 'nil)
    (progn
      (setq it 0)
      (loop for c in strand do
        (if (>= it (+ (- (length strand) (length check)) 1))
          (progn
            (return 'nil)
            (return))
          (progn
            (setq snippet (subseq strand it (+ it (length check))))
            (if (equal snippet check)
              (progn
                (return t)))))
        (setq it (+ it 1))))))

(print (appearsp '(A F G C T) '(G F)))

(defun coversp (test strand)
(if (> (length test) (length strand))
  (return nil)
  (progn
    (setq remainder (rem (length strand) (length test)))
    (if (not (= remainder 0))
      (return nil)
      (progn
        (setq number-of-its (/ (length strand) (length test)))
        (setq it 0)
        (setq nilholder nil)
        (loop
          (if (= number-of-its 0) (return t))
          (if (appearsp (subseq strand (* it (length test)) (+ (* it (length test)) (length test))) test )
            (progn
              (setq it (+ it 1))
              (setq number-of-its (- number-of-its 1)))
            (return nil))))))))

(print (coversp '(t e) '(t e s t)))

(defun prefix (num strand)
  (if (> num (length strand))
    (print "Please enter a number that is less than the length of the strand")
    (print (subseq strand 0 num))))

(prefix 4 '(A F G C T F))


(defun draw-dna (strand)
  (format t "~%")
  (loop for i from 1 to (* (length strand) 2) do
    (format t "-" #\return i))
  (format t "~%")
  (loop for i from 1 to (length strand) do
    (format t "! " #\return i))
  (format t "~%")
  (loop for c in strand do
    (format t "~a " c))
  (format t "~%")
  (loop for i from 1 to (length strand) do
    (format t ": " #\return i))
  (format t "~%")
  (loop for c in strand do
    (format t "~a " (complement-base c)))
  (format t "~%")
  (loop for i from 1 to (length strand) do
    (format t "! " #\return i))
  (format t "~%")
  (loop for i from 1 to (* (length strand) 2) do
    (format t "-" #\return i))
  (format t "~%"))

(draw-dna '(A G T))

;(defun testDraw ()
;  (loop for i from 0 to 10 do
;        (format t "-" #\return)))
;
;(testDraw)

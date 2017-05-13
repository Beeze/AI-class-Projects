;COMPLEMENT-BASE takes a base as input and returns the
;matching base. (COMPLEMENT-BASE 'A) should return T and so
;forth for each base.

(defun complement-base (base)
  (case base
    ('A (quote T))
    ('T (quote A))
    ('G (quote C))
    ('C (quote G))))

(print (complement-base 'A))

;COMPLEMENT-STRAND returns the complementary strand of a
;sequence of single-stranded DNA. (COMPLEMENT-STRAND '(A G G T))
;should return (T C C A).

(defun complement-strand (strand)

  ;Create an empty list to hold our DNA strands complement
  (setq complement-strand '())

  ;Go through each element of the strand list, and complement the base.
  (loop for c in strand do
    (setq complement-strand (append  complement-strand (list (complement-base c)))))

  ;Print the complement of the given strand
  (print complement-strand))

(defvar *strand* '(A G T C))
(complement-strand *strand*)


;MAKE-DOUBLE takes a single strand DNA and returns a double-
;stranded version. (MAKE-DOUBLE '(G G A C T)) should return
;((G C) (G C) (A T) (C G) (T A)).

(defun make-double (strand)
  ;Create an empty list to hold our double DNA strands
  (setq double-strand '())

    ;go through each base in the strand
    ;create a list composed on each base and it's complement-base
    ;append the newly created list to our double-strand list.
    (loop for c in strand do
      (setq double-strand (append double-strand (list (list c (complement-base c))))))
  (print double-strand))

(make-double *strand*)

;COUNT-BASES counts the number of bases of each type in
;either single- or double-stranded DNA and returns the result
;as a table. (COUNT-BASES '((G C) (A T) (T A) (C G))) should
;return ((A 2) (T 2) (G 2) (C 2)) and
;(COUNT-BASES '(A G T A C T C T)) should return
;((A 2) (T 3) (G 1) (C 2)).

(defun count-bases (strand)
  ;Initialize variables that will count the number of occurrences of a base
  (setq numA 0)
  (setq numC 0)
  (setq numG 0)
  (setq numT 0)

  ;Go through each base in the strand
  ;increment the corresponding bases counter.
  (loop for c in strand do
    (case c
      ('A (setq numA (+ numA 1)))
      ('C (setq numC (+ numC 1)))
      ('G (setq numG (+ numG 1)))
      ('T (setq numT (+ numT 1)))))

  ;Print the bases with their count.
  (print (list
    (list 'A numA)
    (list 'C numC)
    (list 'G numG)
    (list 'T numT))))

(count-bases *strand*)

;PREFIXP returns T if one strand of DNA is a prefix of
;another and NIL otherwise. For example, (G T C) is a prefix
;of (G T C A T) but not of (A G G T C).
(defun prefixp (strand prefix)

  ;make sure the prefix is not bigger than the strand itself.
  (if (> (length prefix) (length strand))
    (print nil)
    (progn

      ;take the prefix
      ;Get a subsequence of the strand
      ;Compare the given prefix to the beginning of the strand.
      (setq len (length prefix))
      (setq beginning-of-strand (subseq strand 0 len))
      (if (equal beginning-of-strand prefix)
        (print t)
        (print nil)))))

(prefixp '(A G C T) '(A G C))

;APPEARSP returns T if one DNA strand appears anywhere within
;another. For example, (C A T) appears in (T C A T G) but not
;in (T C C G T A). Hint: If X appears in Y then X is a prefix
;of Y or (CDR Y) or (CDR (CDR Y)) or ...

(defun appearsp (strand check)

  ;make sure the prefix is not bigger than the strand itself.
  (if (> (length check) (length strand))
    (return nil)

    (progn
      (setq it 0)
      ; Go through each character in the strand
      (loop for c in strand do
        ;Check to make sure we haven't gone outside the bounds of the strand
        (if (>= it (+ (- (length strand) (length check)) 1))
          (progn
            (return nil)
            (return))
          ;grab a snippet of the strand, compare it to the check list.
          (progn
            (setq snippet (subseq strand it (+ it (length check))))
            (if (equal snippet check)
              (progn
                (return t)))))
        ;Increment our counter
        (setq it (+ it 1))))))

(print (appearsp '(A F G C T) '(G F)))

;COVERP returns T if its first input, repeated some number of
;times, matches all of its second input. For example, (A G C)
;covers (A G C A G C A G C) but not (A G C T T G). You may
;assume neither input will be NIL.

(defun coversp (test strand)

;make sure the test list is not bigger than the strand itself.
(if (> (length test) (length strand))
  (return nil)
  (progn

    ; Check the remainder of dividing the strand length by the test length.
    ; If the remainder is 0, we know the test list can go into the strand
    ; an equal number of times
    (setq remainder (rem (length strand) (length test)))
    (if (not (= remainder 0))
      (return nil)

      ; Test list can fit into the strand
      ; We're going to go through, and check to see if the test strand
      ; can be repeated to span the given strand.
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

;PREFIX returns the leftmost N bases of a DNA strand.
;(PREFIX 4 '(C G A T T A G)) should return (C G A T).
(defun prefix (num strand)
  (if (> num (length strand))
    (print "Please enter a number that is less than the length of the strand")
    (print (subseq strand 0 num))))

(prefix 4 '(A F G C T F))

;(extra credit) DRAW-DNA  takes a single-stranded DNA sequence
;as input and draws it and its complementary strand.
;(DRAW-DNA '(A G G T C A T T G) should produce the following
;output:
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

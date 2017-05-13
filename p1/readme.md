Your first programming assignment will give you an 
opportunity to explore symbolic computation as you write a series 
of functions to explore some properties of single- and double-
stranded DNA. A strand of DNA can be modeled using a linked list 
and is composed of four bases (adenine, thymine, guanine, and 
cytosine). The list (A G G T C A T T G) corresponds to a strand 
that is nine bases long, using the first letter of each name as a 
symbol for the base. Each of the four bases has a complement with 
which it can form a pair. Adenine pairs with thymine, while 
guanine pairs with cytosine. Two complementary single-strands of 
DNA can combine to form a double-stranded DNA. The strands (A G G 
T C A T T G) and (T C C A G T A A C) are complementary. You are 
to write and test the following LISP functions:
 
  1. COMPLEMENT-BASE takes a base as input and returns the      
     matching base. (COMPLEMENT-BASE 'A) should return T and so      
     forth for each base.
 
  2. COMPLEMENT-STRAND returns the complementary strand of a      
     sequence of single-stranded DNA. (COMPLEMENT-STRAND '(A G G T)) 
     should return (T C C A).
 
  3. MAKE-DOUBLE takes a single strand DNA and returns a double-     
     stranded version. (MAKE-DOUBLE '(G G A C T)) should return      
     ((G C) (G C) (A T) (C G) (T A)).
 
  4. COUNT-BASES counts the number of bases of each type in      
     either single- or double-stranded DNA and returns the result      
     as a table. (COUNT-BASES '((G C) (A T) (T A) (C G))) should      
     return ((A 2) (T 2) (G 2) (C 2)) and 
     (COUNT-BASES '(A G T A C T C T)) should return 
     ((A 2) (T 3) (G 1) (C 2)).
 
  5. PREFIXP returns T if one strand of DNA is a prefix of      
     another and NIL otherwise. For example, (G T C) is a prefix 
     of (G T C A T) but not of (A G G T C).
 
  6. APPEARSP returns T if one DNA strand appears anywhere within      
     another. For example, (C A T) appears in (T C A T G) but not      
     in (T C C G T A). Hint: If X appears in Y then X is a prefix      
     of Y or (CDR Y) or (CDR (CDR Y)) or ...
 
  7. COVERP returns T if its first input, repeated some number of      
     times, matches all of its second input. For example, (A G C)      
     covers (A G C A G C A G C) but not (A G C T T G). You may      
     assume neither input will be NIL.
 
  8. PREFIX returns the leftmost N bases of a DNA strand. 
     (PREFIX 4 '(C G A T T A G)) should return (C G A T).
 
  9. KERNEL returns the shortest prefix of a DNA strand that can      
     be repeated to cover the strand. (KERNEL '(A G C A G C A G C)) 
     should return (A G C). (KERNEL '(A A A A A)) should      
     return (A). (KERNEL '(A G G T C)) should return (A G G T C).           
 
 10.  (extra credit) DRAW-DNA  takes a single-stranded DNA sequence 
      as input and draws it and its complementary strand.  
      (DRAW-DNA '(A G G T C A T T G) should produce the following 
      output:


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

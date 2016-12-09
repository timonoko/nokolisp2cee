
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (6 / 2 - 2005) (0 : 16 : 33 38))
(defq *package* RB)

(defun rb
 (x msb)
 (if
  (zerop msb)
  0
  (+
   (/ x msb)
   (* 2 (rb (remainder x msb) (/ msb 2))))))

(defq RB (rb RB))

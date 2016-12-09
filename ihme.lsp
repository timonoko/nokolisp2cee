
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (7 / 8 - 2006) (1 : 9 : 38 18))
(defq *package* IHME)

(defun ihme ()
 (print
  (compress
   (print
    (reverse (explode (read)))
    (print (reverse (explode (read)))))))
 (cr))

(defq IHME (ihme IHME))

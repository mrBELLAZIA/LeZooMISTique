(defun long (l)
                   (cond ( (null l) 0)
                         ( (atom l) nill)
                         ( t (+ 1 (long (cdr l))))
                         )
             )

(defun grand (l1 l2)
                   (cond ((< (long l1) (long l2)) l2)
                         ( t l1)
                         )
                   )

(defun petit (l1 l2)
                   (cond ((> (long l1) (long l2)) l2)
                         ( t l1)
                         )
                   )


(defun put(symbol prop value)
  (cond ((null (get symbol prop)) (setf (get symbol prop) (list value))) 
        (t (setf (get symbol prop) (union (list value) (get symbol prop))))
  )
)
;(defun put(symbol prop value)
;  (setf (get symbol prop) value))

(defun membre (l a)
                   (cond ( (null l) nil)
                         ( (atom l) nil)
                         ( (equal a (car l)) t)
                         ( t (membre (cdr l) a))
                         )
                   )

(defun union2 (l1 l2)
  (cond ((null l1) l2)
        ((membre l2 (car l1)) (union l2 (cdr l1)))
        ( t (union (cdr l1) (cons l2 (car l1))))
        )
  )

(defun intersect2 (l1 l2)
              (cond ((null (grand l1 l2)) nil)
                    ((membre (grand l1 l2) (car (petit l1 l2)))(cons (car (petit l1 l2)) (intersect2 (cdr (grand l1 l2)) (cdr (petit l1 l2)))))
                    ( t (intersect2 (cdr (grand l1 l2) ) (cdr (petit l1 l2) )))
                    )
              )

(defun dernier (l)
                   (cond ((null (cdr l)) (car l))
                         ( t (dernier (cdr l)))
                         )
                   )

(defun renverse (l)
    (cond ((atom l) l)
          (t (cons (renverse (dernier l)) (renverse (remove (dernier l) l))))
))

(defun si (cond alors sinon)
  (cond ((eval cond) (eval alors))
        (t (eval sinon))
))

(setq NL nil)
(setq RL nil)

(defun nodelist ()
  NL)
 
(defun relationlist ()
  RL)

(defun nodep (n)
  (membre (nodelist) n))

(defun relationpbis (r l)
  (cond ((null l) nil)(put 'a 'mange 'herbe)
        ((not (equal (get (car l) r) nil)) t)
        (t (relationpbis r (cdr l)))
  )
)

(defun relationp (r)
  (relationpbis r NL))

(defun defnodelist (l)
  (setq NL (union l (nodelist))))

(defun defnode (node)
  (defnodelist (list node))
)
(defun putrelation (n1 r n2)
  (put n1 'rlist r)
  (put n1 r n2)
  (setq RL (union RL (list r))))


(defun getvalue (n r) 
  (get n r))


;tests

(putrelation 'chien 'pattes 3)
(putrelation 'cheval 'queue 1)
(putrelation 'chat 'mignon 0)
(defnodelist '(cheval chien chat))
  

(defun test ()
  nil)
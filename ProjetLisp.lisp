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
  (setf (get symbol prop) value))

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

(defun relationp (r)
  (membre (relationlist) r))

(defun defnodelist (l)
  (setq NL (union l (nodelist))))

(defun defnode (node)
  (defnodelist (list node))
)


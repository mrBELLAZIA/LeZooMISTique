;==========================
;METHODES RESEAU SEMANTIQUE
;==========================


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


(defun putUnitaire(symbol prop value)
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

(defun relationpbis (r l)
  (cond ((null l) nil)
        ((equal (car l) r) t)
        (t (relationpbis r (cdr l)))
  )
)

(defun relationp (r)
  (relationpbis r RL))

(defun defnodelist (l)
  (setq NL (union l (nodelist))))

(defun defnode (node)
  (defnodelist (list node))
)
(defun putrelation (n1 r n2)
  (put n1 'rlist r)
  (put n1 r n2)
  (setq RL (union RL (list r))))

(defun rlnoeuds (n)
  (get n 'rlist))

(defun reclinkednodes (n l)
  (cond ((null l) nil)
        (t (union (get n (car l)) (reclinkednodes n (cdr l))))))

(defun linkednodes (n)
  (reclinkednodes n (rlnoeuds n)))

(defun getvalue (n r)
  (cond ((not (equal (get n r) nil)) (get n r))
        ((not (equal (get n 'est_un) nil)) (getvalue (car (get n 'est_un)) r))
        (t nil)
  )
)

(defun removevalue (n1 r n2)
  (setf (get n1 r) (remove n2 (get n1 r)))
)

;==============================
;METHODE POUR LE SYSTEME EXPERT
;==============================
(setq regles nil)
(setq arret nil)
(setq BF nil)


;ACCESSEURS
(defun arretcond()
  arret
)

(defun basefait()
  BF
)

(defun reglesList ()
  regles)


;METHODES SUR RELGES
(defun newrule(rule conditionP actionP poids)
  (setq regles (union (list rule) (reglesList)))
  (setf (get rule  'condition) conditionP)
  (setf (get rule  'action) actionP)
  (setf (get rule  'poids) poids)
)

;METHODE SUR CONDITIONS D'ARRET
(defun setArret(conditionP)
  (setq arret conditionP)
)

;METHODE SUR BASE DE FAITS
(defun addFait(nom valeur)
  (set nom  valeur)
  (setq BF (union (list nom) (basefait)))

)


;====================
;INSTANCIATION RESEAU
;====================
;listes des noeuds
(defnodelist '(felin canide oiseau bovin chat poisson poisson_chat mouette aigle boeuf chien loup))

;listes des relations
(putrelation 'chien 'est_un 'canide)
(putrelation 'chien 'attaque 'loup)
(putrelation 'aigle 'attaque 'chien)
(putrelation 'chien 'mange 'boeuf)
(putrelation 'loup 'est_un 'canide)
(putrelation 'aigle 'est_un 'oiseau)
(putrelation 'mouette 'est_un 'oiseau)
(putrelation 'mouette 'mange 'poisson_chat)
(putrelation 'poisson_chat 'est_un 'poisson)
(putrelation 'chat 'est_un 'felin)
(putrelation 'chat 'mange 'poisson_chat)
(putrelation 'boeuf 'a_peur 'chat)
(putrelation 'boeuf 'est_un 'bovin)
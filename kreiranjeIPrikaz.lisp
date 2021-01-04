(setf tabla '())
(setf brojevi '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (A 10) (B 11) (C 12) (D 13) (E 14) (F 15)))


;-------------------------------------------------------------KREIRANJE-------------------------------------------------------------

(defun ispuniJedanRed(el n brojac)
	(cond
		( (= brojac 0) '())
		( T (cons (ispuniStub el n) (ispuniJedanRed el n (1- brojac) )) )		
	)
)

(defun ispuniStub (el n) ;vraca Stub 
;lista ima n elemenata i svi su isti kao parametar el
	(cond
		( (= n 0) '())
		( T (cons el (ispuniStub el (1- n) )) )		
	)
)

(defun kreirajTablu (el n brojac)
    (cond
		( (= brojac 0) '())
		( T (cons (ispuniJedanRed el n n) (kreirajTablu el n (1- brojac) )) )		
	)
)

(defun novaIgra (n igracXiliO)
    ;X uvek igra prvi
	(setq napotezu 'X)
    ;ako je igracXiliO postavljeno na 'X, znaci da covek igra prvi, a ako je postavljeno na 'O,
    ;znaci da racunar igra prvi
    ;u sledecim fazama projekta, u zavisnosti od ove promenljive i promenljive napotezu, odredjuje se
    ;da li je covek na potezu ili racunar
    (setq igrac igracXiliO)
    ;nakon svakog poteza, brojacPoteza se inkrementira za 1
    ;kada brojacPoteza dodje do 64, znaci da je kraj igre
    (setq brojacPoteza '0)
	(setq dimTable n)
	
    (setq tabla (kreirajTablu '- n n)) 
    ;(let ((slova '(0 1 2 3 4 5 6 7 8 9 A B C D E F)))
    ;(prikaziTablu tabla n slova)
)

;----------------------------------------------------------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------------------------------PRIKAZ----------------------------------------------------------------



;----------------------------------------------------------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;--------------------------------------------------------KRAJ IGRE I POTEZI-------------------------------------------------------------

(defun krajIgre ()
    (eq brojacPoteza '64)
)

(defun nadjiJedanRed (brReda matrica)
    (cond
        ((= brReda 0) (car matrica))
        (T (nadjiJedanRed (1- brReda) (cdr matrica)))
        
    )
)

(defun proveriValidan(stub)
    (cond
        ((null stub) '())
        (T (cond
                ((eq (car stub) '-) T)
                (T (proveriValidan (cdr stub)))
            ))
    )
)

(defun izvuciBroj(lista)
    (car (cdr lista))
)

(defun proveriPotezAtom (potez)
    (proveriValidan (nadjiJedanRed  (- (izvuciBroj(assoc potez brojevi)) (* (floor (izvuciBroj(assoc potez brojevi)) dimTable) dimTable) ) (nadjiJedanRed (floor (izvuciBroj(assoc potez brojevi)) dimTable) tabla)) )
)

(defun proveriPotezLista (lista)
    (proveriValidan (nadjiJedanRed (izvuciBroj lista) (nadjiJedanRed (car lista) tabla)))
)

(defun potezValidan (potez)
    (cond
      ( (atom potez) (proveriPotezAtom potez))
		( T (proveriPotezLista potez) )
    )
)

;----------------------------------------------------------------------------------------------------------------------------------------
(defun vratiRed(lista brojac)
    (cond
        ((null lista) '())
        (t (append (vratielemente (reverse (car lista)) brojac) (vratiRed (cdr lista) brojac)))
    )
)
(defun vratielemente(lista brojac)
    (cond
        ((= brojac 0) '())
        (t (reverse (cons (caar lista) (reverse (vratielemente (cdr lista) (1- brojac))))))
    )
)

(defun prikaziTablu(lista brojac &optional(p 1))
    (cond
        ((= brojac 0) '())
        ((= p brojac) (append  (vratiRed lista p) (prikaziTablu (brisiIzSvakogReda lista p) (1- brojac) (1- p))))
        ; (T (cons (caar (cdddar lista))(prikaziTablu (cdr lista))))
        ;(T (cons (append (list (append  (reverse (cdr (reverse (car lista)))) (list (cdr (car (reverse (car lista))))))) (prikaziTablu (cdr lista)))))
        (T (append (vratiRed lista p) (prikaziTablu (brisiIzSvakogReda lista p) brojac (1+ p))))

    )
)

(defun brisiRed(lista brojac)
    (cond
        ;((null (car lista)) (reverse (cdr lista)))
        ((= brojac 0) (reverse lista))
        (t (remove nil (reverse (append (list (cdar lista)) (reverse (brisiRed (cdr lista) (1- brojac)))))))
    )
)

(defun brisiIzSvakogReda(lista brojac)
    (cond
        ((null lista) '())
        (t (append (list (brisiRed (reverse (car lista)) brojac)) (brisiIzSvakogReda (cdr lista) brojac)))
    )
)

;(novaIgra 4 4)
(setq tablaa '(((25 41 53 61) (13 26 42 54) (5 14 27 43) (1 6 15 28)) 
               ((29 44 55 62) (16 30 45 56) (7 17 31 46) (2 8 18 32)) 
               ((33 47 57 63) (19 34 48 58) (9 20 35 49) (3 10 21 36))
               ((37 50 59 64) (22 38 51 60) (11 23 39 52) (4 12 24 40))))
(trace prikaziTablu)
(print (prikaziTablu tablaa 4))

;(trace vratielemente)
;(print (vratielemente (reverse '((25 41 53 61) (13 26 42 54) (5 14 27 43) (1 6 15 28))) '2))
;(print (vratiRed '(((25 41 53 61) (13 26 42 54) (5 14 27 43) (1 6 15 28)) 
                 ; ((29 44 55 62) (16 30 45 56) (7 17 31 46) (2 8 18 32)) 
                 ; ((33 47 57 63) (19 34 48 58) (9 20 35 49) (3 10 21 36))
                 ; ((37 50 59 64) (22 38 51 60) (11 23 39 52) (4 12 24 40))) '2))
;; (print (brisiRed (reverse'((25 41 53 61) (13 26 42 54) (5 14 27 43) (1 6 15 28))) '2))
;; (print (brisiIzSvakogReda '(((25 41 53 61) (13 26 42 54) (5 14 27 43) (1 6 15 28)) 
;;                ((29 44 55 62) (16 30 45 56) (7 17 31 46) (2 8 18 32)) 
;;                ((33 47 57 63) (19 34 48 58) (9 20 35 49) (3 10 21 36))
;;                ((37 50 59 64) (22 38 51 60) (11 23 39 52) (4 12 24 40))) '4))
;(print (potezValidan '1))

;; (print (vratielemente '(((25 41 53 61) (13 26 42 54) (5 14 27 43) (1 6 15 28))) '1))
;; (print (brisiIzSvakogReda '((25 41 53 61) (13 26 42 54) (5 14 27 43) (1 6 15 28)) '1))
;; (print (vratiRed '(((25 41 53 61) (13 26 42 54) (5 14 27 43) (6 15 28))) '2))







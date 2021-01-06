(setf tabla '())
(setf brojevi '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (A 10) (B 11) (C 12) (D 13) (E 14) (F 15)))

;----------------------------------------------------------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;---------------------------------------------------------------KREIRANJE----------------------------------------------------------------

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
    (setq slova '((0 1 2 3)(4 5 6 7)(8 9 A B)(C D E F)))
    (prikaziSlova slova)
    (stampa (prikaziTablu tabla n) n)
    (prikaziSlova slova)
)

;----------------------------------------------------------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------------PRIKAZ------------------------------------------------------------------

;Ove funkcije sluze za sortiranje kreiranje liste kako bi se prosledila funkciji za konkretno prikazivanje
(defun vratiRed(lista brojac)
    (cond
        ((null lista) '())
        (t (append (list (vratielemente (reverse (car lista)) brojac)) (vratiRed (cdr lista) brojac)))
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
        ((= p brojac) (append  (list (vratiRed lista p)) (prikaziTablu (brisiIzSvakogReda lista p) (1- brojac) (1- p))))
        ; (T (cons (caar (cdddar lista))(prikaziTablu (cdr lista))))
        ;(T (cons (append (list (append  (reverse (cdr (reverse (car lista)))) (list (cdr (car (reverse (car lista))))))) (prikaziTablu (cdr lista)))))
        (T (append (list (vratiRed lista p)) (prikaziTablu (brisiIzSvakogReda lista p) brojac (1+ p))))

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

;Funkcije na dole sluze za iscrtavanje same table
(defun stampaj_listu (l brojac) 
    (cond
        ((null (cdr l)) (format t "~VT" (1- brojac)) (lepPrikazliste (car l)))
		(T (format t "~VT" (1- brojac)) (lepPrikazliste (car l)) (stampaj_listu (cdr l) (+ brojac 5)))
    )
)

(defun stampaj_listu_dole (l brojac) 
    (cond
        ((null l) (format t "~VT" (1+ brojac)) (lepPrikazliste (car l)))
		(T (format t "~VT" (1+ brojac )) (lepPrikazliste (car l)) (stampaj_listu_dole (cdr l) (+ brojac 5)))
    )
)

(defun stampa(lista brojac &optional(p 0))
    (cond
        ((null lista) '())
        ((= brojac p) (format t "~%" (stampaj_listu_dole (car lista) 0)) (stampa (cdr lista) 0 0))
        (t (format t "~%" (stampaj_listu (car lista) (1+ brojac))) (stampa (cdr lista) (1- brojac)))
    )
)

(defun lepPrikazliste(lista)
    (cond 
        ((null lista) '())
        (t (format t "~a" (car lista)) (lepPrikazliste (cdr lista)))
    )
)

(defun prikaziSlova(lista)
    (cond 
        ((null lista) (format t "~%"))
        (t (format t "~VT" 1)(prikaziSlovaPomocna (car lista)) (prikaziSlova (cdr lista)))
    )
)

(defun prikaziSlovaPomocna(lista)
    (cond
        ((null lista) '())
        (T (format t "~a" (car lista))(prikaziSlovaPomocna(cdr lista)))
    )
)
;(trace prikaziSlovaPomocna)

;----------------------------------------------------------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;--------------------------------------------------------KRAJ IGRE I POTEZI--------------------------------------------------------------

;; (defun krajIgre (brojacPoteza lista)
;;     (cond
;;         ((= brojacPoteza 64)(proveriPobednika(lista)))
;;     )
;; )
(defun Pobednik(lista)
    (if (> (xILIo (proveriPobednika lista) 'X) (xILIo (proveriPobednika lista) 'O)) (format t "Pobednik je X!") (format t "Pobednik je O!"))
) 

(defun xILIo(lista element)
    (cond 
        ((null lista) 0)
        (t (if (equalp (car lista) element) (1+ (xILIo (cdr lista) element)) (xILIo (cdr lista) element)))
    )
)

(defun proveriPobednika(lista)
    (append (append (proveriZaKolone lista) (proveriZaKolone lista)) (proveriZaKolone lista))
)
;; (trace proveriPobedika)

(defun proveriZaKolone(lista)
    (cond
        ((null lista) '())
        (t (remove nil (append (proveriZaKolonePomocna(car lista)) (proveriZaKolone(cdr lista)))))
    )
)

(defun proveriZaKolonePomocna(lista)
    (cond
        ((null lista) '())
        (t (cons (proveriJednakostListe(car lista)(caar lista)) (proveriZaKolonePomocna(cdr lista))))
    )
)

(defun proveriJednakostListe(lista element)
    (cond 
        ((null lista) element)
        ((equalp (car lista) element) (proveriJednakostListe (cdr lista) element))
    )
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------POVLACENJE POTEZA----------------------------------------------------------------

(defun vratiElement(element ubacenElement)
    (cond
    ((eq element '-) (cond (ubacenElement '-) (T napotezu)))
    (T element)
    )
)


(defun ubaciNaStapic(stapic ubacenElement)
    	;(cond
		;( (eq (car stapic) '-) napotezu)
		;( T (cons (ispuniStub el n) (ispuniJedanRed el n (1- brojac) )) )		
	;)

    (cond 
    ((null stapic) '())
    ((eq (car stapic) '-) ( cons (vratiElement (car stapic) ubacenElement ) (ubaciNaStapic (cdr stapic) 1 ) ))
    ( T ( cons (vratiElement (car stapic) ubacenElement ) (ubaciNaStapic (cdr stapic) '() ) ) )
    )
)

(defun nadjiStapic (vrsta brojac)
    (cond
    ((null vrsta) '())
    ((= brojac 0) (cons ( ubaciNaStapic (car vrsta) '()) ( nadjiStapic (cdr vrsta) (1- brojac)) ) )
    ( T (cons (car vrsta) (nadjiStapic (cdr vrsta) (1- brojac)) ))
    )
)

(defun nadjiVrstu (tabela listaX brojacY)
     (cond
    ((null tabela) '())
    ((= listaX 0) (cons ( nadjiStapic (car tabela) brojacY) ( nadjiVrstu (cdr tabela) (1- listaX) brojacY) ) )
    ( T (cons (car tabela) (nadjiVrstu (cdr tabela) (1- listaX) brojacY) ))
    )
)

(defun konvertujUKoordinate (broj)
    (list (floor broj dimTable) (mod broj dimTable))
)

(defun konvertujPotez (potez)
    (konvertujUKoordinate (izvuciBroj(assoc potez brojevi)))
)

(defun povuciPotez (potez)
    (cond 
        ((atom potez) (nadjiVrstu tabla (car (konvertujPotez potez)) (car (cdr (konvertujPotez potez)))))
        (T (nadjiVrstu tabla (car potez ) (car (cdr potez))))
    )
)

;----------------------------------------------------------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------POZIV FUNKCIJA----------------------------------------------------------------

;(novaIgra 4 4)
;; (setq tablaa '(((25 41 53 61) (13 26 42 54) (5 14 27 43) (1 6 15 28)) 
;;                ((29 44 55 62) (16 30 45 56) (7 17 31 46) (2 8 18 32)) 
;;                ((33 47 57 63) (19 34 48 58) (9 20 35 49) (3 10 21 36))
;;                ((37 50 59 64) (22 38 51 60) (11 23 39 52) (4 12 24 40))))
(setq tablaa '(((X X X X) (X X X X) (O O O O) (O O O O)) 
               ((- - X -) (- - x -) (O O O O) (- -x - -)) 
               ((- - x -) (- O - -) (O O O O) (- - X -))
               ((- - x- -) (- -x - -) (- x- - -) (- O - -))))

;(trace preurediZaRedPomocna)
;(print (preurediZaRed tablaa))
(Pobednik tablaa)

;(trace prikaziTablu)
;(print (prikaziTablu tablaa 4))
;(print (potezValidan '1))
;(trace stampaj_listu)
;(stampa (prikaziTablu tablaa 4) 4)

;(novaIgra '4 'O)
;; (princ tabla)
;; (setq tabla (povuciPotez '(0 0)))
;; (princ tabla)
;; (princ (atom '1))
;; (princ (konvertujPotez '1))
;; (princ (floor 1 dimTable))
;; (princ (mod 1 dimTable))
;; (princ (list 0 1 ))
;; (setq tabla (povuciPotez '1))
;; (princ tabla)
;; (setq tabla (povuciPotez 'C))
;; (princ tabla)

;; (trace daLiSu)
;; (print (daLiSu '(o o o x) 'o))






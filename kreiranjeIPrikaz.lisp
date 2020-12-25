(setf tabla '())
(setf brojevi '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (A 10) (B 11) (C 12) (D 13) (E 14) (F 15)))


(defun ispuniVrstu(el n brojac)
	(cond
		( (= brojac 0) '())
		( T (cons (ispuniListu el n) (ispuniVrstu el n (1- brojac) )) )		
	)
)

(defun izvuciBroj(lista)
    (car (cdr lista))
)

(defun ispuniListu (el n) ;vraca listu 
;lista ima n elemenata i svi su isti kao parametar el
	(cond
		( (= n 0) '())
		( T (cons el (ispuniListu el (1- n) )) )		
	)
)

(defun kreirajTabluPom (el n brojac)
    (cond
		( (= brojac 0) '())
		( T (cons (ispuniVrstu el n n) (kreirajTabluPom el n (1- brojac) )) )		
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
	
   (setq tabla (kreirajTabluPom '- n n)) 
)

(defun krajIgre ()
    (eq brojacPoteza '64)
)

(defun nadjiVrstu (brVrste matrica)
    (cond
        ((= brVrste 0) (car matrica))
        (T (nadjiVrstu (1- brVrste) (cdr matrica)))
        
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

(defun proveriPotezAtom (potez)
    (proveriValidan (nadjiVrstu  (- (izvuciBroj(assoc potez brojevi)) (* (floor (izvuciBroj(assoc potez brojevi)) dimTable) dimTable) ) (nadjiVrstu (floor (izvuciBroj(assoc potez brojevi)) dimTable) tabla)) )
)

(defun proveriPotezLista (lista)
    (proveriValidan (nadjiVrstu (izvuciBroj lista) (nadjiVrstu (car lista) tabla)))
)
(defun potezValidan (potez)
    (cond
      ( (atom potez) (proveriPotezAtom potez))
		( T (proveriPotezLista potez) )
    )
)



(novaIgra 4 4)
(setq tabla '(((X X X X) (O O O -) (- - - -) (- - - -)) ((- - - -) (- - - -) (- - - -) (- - - -)) ((- - - -) (- - - -) (- - - -) (- - - -))
 ((- - - -) (- - - -) (- - - -) (- - - -))))
(print (potezValidan '1))







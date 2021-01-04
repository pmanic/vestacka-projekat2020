(defun vratiElement(element)
    (cond
    ((eq element '-) napotezu)
    (T element)
    )
)


(defun ubaciNaStapic(stapic)
    	;(cond
		;( (eq (car stapic) '-) napotezu)
		;( T (cons (ispuniStub el n) (ispuniJedanRed el n (1- brojac) )) )		
	;)

    (cond 
    ((null stapic) '())
    ( T ( cons (vratiElement (car stapic) ) (ubaciNaStapic (cdr stapic) ) ) )
    )
)

(defun nadjiStapic (vrsta brojac)
    (cond
    ((null vrsta) '())
    ((= brojac 0) (cons ( ubaciNaStapic (car vrsta)) ( nadjiStapic (cdr vrsta) (1- brojac)) ) )
    ( T (cons (car vrsta) (nadjiStapic (cdr vrsta) (1- brojac)) ))
    )
)

(defun nadjiVrstu (tabela brojacX brojacY)
     (cond
    ((null tabela) '())
    ((= brojacX 0) (cons ( nadjiStapic (car tabela) brojacY) ( nadjiVrstu (cdr vrsta) (1- brojacX) brojacY) ) )
    ( T (cons (car tabela) (nadjiVrstu (cdr tabela) (1- brojacX) brojacY) ))
    )
)

(defun konvertujUKoordinate (broj)
    (cons (floor broj n) (mod broj n))
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

(defun linha (y tabuleiro)
    (nth y tabuleiro)
)

(defun celula (y x tabuleiro)
    (nth x (linha y tabuleiro))
)

(defun lista-numeros ( &optional (n 100))
    (loop for i upto (- n 1) collect i)
)

;;(remover-se #'(lambda (x) (= x 0)) '(1 2 0 2 0 4))
(defun remover-se(pred lista)
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista))))))

(defun pick-random (lista)
    (cond
        ((= (length lista) 0) NIL)
        (T (nth (random (length lista)) lista))
    )
)

(defun baralhar (remover &optional (nova-lista '()))
    (let ((num (pick-random remover)))
        (cond 
            ((= (length remover) 0) nova-lista)
            (T (baralhar (remover-se #'(lambda (x) (= x num)) remover) (cons num nova-lista)))
        ) 
    )
)

(defun tabuleiro-aleatorio (&optional (lista (baralhar(lista-numeros))) (n 10))
    (cond
        ((null lista) nil)
        (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
    )
)

(defun substituir-posicao (indice lista &optional (valor NIL))
    (cond
      ((> indice (- (length lista) 1)) NIL)
      (T (append (subseq lista 0 indice) (cons valor (subseq lista (+ indice 1)))))
    )
)

(defun substituir (x y lista &optional (valor NIL))
   (cond 
    ((> y (- (length lista) 1)) NIL)
    (T (append (subseq lista 0 y) 
            (cons (substituir-posicao x (linha y lista) valor) (subseq lista (+ y 1)))))
    )
)

(defun valuep (lista &optional (value T))
    (member value lista)
)

(defun posicao (value lista &optional (y 0))
    (cond
        ((not lista) nil) 
        ((= (length lista) 0) nil)
        ((valuep (car lista) value) (list (position value (car lista)) y))
        (T (posicao value (cdr lista) (+ y 1)))
    )
)

(defun posicao-cavalo (lista)
    (posicao T lista)
)

(defun simetrico (num)
    (multiple-value-bind (q r) (floor num 10) (+ (* r 10) q))
)

(defun substituir-simetrico (num lista)
    (let ((pos (posicao (simetrico num) lista)))
        (substituir (first pos) (second pos) lista)
    )
)

(defun nova-posicao (pos offset-x offset-y)
  (cond
       ((not pos) NIL)
       (T (list (+(first pos) offset-x) (+(second pos) offset-y)))
   )
)

(defun posicao-valida (lista pos)
    (cond
        ((not lista) NIL)
        ((not pos) NIL)
        ((or (< (first pos) 0) (< (second pos) 0)) NIL)
        ((or (> (first pos) 9) (> (second pos) 9)) NIL)
        ((not (celula (second pos) (first pos) lista)) NIL)
        (T T)
    )
)

(defun operador (lista x y)
    (let (
            (pos (posicao-cavalo lista))
        )
        (cond
            ((not pos) NIL)
            ((not (posicao-valida lista (nova-posicao pos x y))) NIL)
            (T 
                (substituir (first (nova-posicao pos x y)) (second (nova-posicao pos x y)) 
                    (substituir-simetrico 
                        (celula (second (nova-posicao pos x y)) (first (nova-posicao pos x y)) lista) 
                        (substituir (first pos) (second pos) lista NIL))
                T)
            )
        )
    )   
)

(defun operador-1 (lista)
    (operador lista -1 2)
)
(defun operador-2 (lista)
    (operador lista 1 2)
)
(defun operador-3 (lista)
    (operador lista 2 1)
)
(defun operador-4 (lista)
    (operador lista 2 -1)
)
(defun operador-5 (lista)
    (operador lista 1 -2)
)
(defun operador-6 (lista)
    (operador lista -1 -2)
)
(defun operador-7 (lista)
    (operador lista -2 -1)
)
(defun operador-8 (lista)
    (operador lista -2 1)
)

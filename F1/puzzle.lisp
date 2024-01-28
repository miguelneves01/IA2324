;; Returns the row at index y in the given board.
(defun linha (y tabuleiro)
    (nth y tabuleiro)
)

;; Returns the cell at position (y, x) in the given board.
(defun celula (y x tabuleiro)
    (nth x (linha y tabuleiro))
)

;; Generates a list of numbers from 0 to n-1 (default: 100).
(defun lista-numeros ( &optional (n 100))
    (loop for i upto (- n 1) collect i)
)

;; Generates a list of doubles from 0 to n-1 (default: 10).
(defun lista-duplos ( &optional (n 10))
    (loop for i upto (- n 1) collect (+ (* i 10) i))
)

;; Removes all elements from the list that satisfy the given predicate.
(defun remover-se(pred lista)
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista))))))

;; Picks a random element from the list.
(defun pick-random (lista)
    (cond
        ((= (length lista) 0) NIL)
        (T (nth (random (length lista)) lista))
    )
)

;; Shuffles the list by randomly picking elements.
(defun baralhar (remover &optional (nova-lista '()))
    (let ((num (pick-random remover)))
        (cond 
            ((= (length remover) 0) nova-lista)
            (T (baralhar (remover-se #'(lambda (x) (= x num)) remover) (cons num nova-lista)))
        ) 
    )
)

;; Generates a random board by shuffling a list of numbers.
(defun tabuleiro-aleatorio (&optional (lista (baralhar(lista-numeros))) (n 10))
    (cond
        ((null lista) nil)
        (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
    )
)

;; Replaces the element at the given index in the list with the specified value.
(defun substituir-posicao (indice lista &optional (valor NIL))
    (cond
      ((> indice (- (length lista) 1)) NIL)
      (T (append (subseq lista 0 indice) (cons valor (subseq lista (+ indice 1)))))
    )
)

;; Replaces the element at position (x, y) in the list with the specified value.
(defun substituir (x y lista &optional (valor NIL))
   (cond 
    ((> y (- (length lista) 1)) NIL)
    (T (append (subseq lista 0 y) 
            (cons (substituir-posicao x (linha y lista) valor) (subseq lista (+ y 1)))))
    )
)

;; Checks if the given value is present in the list.
(defun valuep (lista &optional (value T))
    (member value lista)
)

;; Returns the position of the given value in the list as a (x, y) pair.
(defun posicao (value lista &optional (y 0))
    (cond
        ((not lista) nil) 
        ((= (length lista) 0) nil)
        ((valuep (car lista) value) (list (position value (car lista)) y))
        (T (posicao value (cdr lista) (+ y 1)))
    )
)

;; Returns the position of the horse in the board as a (x, y) pair.
(defun posicao-cavalo (lista)
    (posicao T lista)
)

;; Returns the symmetric number of the given number.
(defun simetrico (num)
    (multiple-value-bind (q r) (floor num 10) (+ (* r 10) q))
)

;; Checks if the given number is a double.
(defun duplop (num)
    (= num (simetrico num))
)

;; Replaces a random double number in the list with NIL.
(defun substituir-duplo-random (lista-duplos lista)
    (let* ((num (pick-random lista-duplos)))
        (cond 
            ((not lista) NIL)
            ((= (length lista-duplos) 0) lista)
            ((posicao num lista) 
                (substituir 
                    (first (posicao num lista))
                    (second (posicao num lista))
                    lista))
            (T (substituir-duplo-random (remover-se #'(lambda (x) (= x num)) lista-duplos) lista))
        )
    )
)

;; Replaces the maximum double number in the list with NIL
(defun substituir-duplo-max (lista-duplos lista)
    (cond 
        ((not lista) NIL)
        ((= (length lista-duplos) 0) lista)
        ((posicao (first lista-duplos) lista) 
            (substituir 
                (first (posicao (first lista-duplos) lista))
                (second (posicao (first lista-duplos) lista))
                lista))
        (T (substituir-duplo-max (cdr lista-duplos) lista))
    )
)

;; Replaces the symmetric of the given number in the list with NIL.
(defun substituir-simetrico (num lista &optional(maxp T))
    (cond 
        ((and (duplop num) (not maxp)) (substituir-duplo-random (remover-se #'(lambda (x) (= x num)) (lista-duplos)) lista))
        ((duplop num) (substituir-duplo-max (reverse (remover-se #'(lambda (x) (= x num)) (lista-duplos))) lista))
        (T (substituir (first (posicao (simetrico num) lista)) (second (posicao (simetrico num) lista)) lista))
    )
)

;; Calculates the new position by applying the given offset to the current position.
(defun nova-posicao (pos offset-x offset-y)
  (cond
       ((not pos) NIL)
       (T (list (+(first pos) offset-x) (+(second pos) offset-y)))
   )
)

;; Checks if the new position is valid in the given board.
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

;; Moves the horse in the board using the given x and y offsets.
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

;; Moves the horse in the board using the (-1, 2) offset.
(defun operador-1 (lista)
    (operador lista -1 2)
)

;; Moves the horse in the board using the (1, 2) offset.
(defun operador-2 (lista)
    (operador lista 1 2)
)

;; Moves the horse in the board using the (2, 1) offset.
(defun operador-3 (lista)
    (operador lista 2 1)
)

;; Moves the horse in the board using the (2, -1) offset.
(defun operador-4 (lista)
    (operador lista 2 -1)
)

;; Moves the horse in the board using the (1, -2) offset.
(defun operador-5 (lista)
    (operador lista 1 -2)
)

;; Moves the horse in the board using the (-1, -2) offset.
(defun operador-6 (lista)
    (operador lista -1 -2)
)

;; Moves the horse in the board using the (-2, -1) offset.
(defun operador-7 (lista)
    (operador lista -2 -1)
)

;; Moves the horse in the board using the (-2, 1) offset.
(defun operador-8 (lista)
    (operador lista -2 1)
)

;; Sets the initial position of the horse in the board.
(defun operador-inicial (lista &optional (casa 0))
    (substituir 
        casa
        0
     lista T)
)
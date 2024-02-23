(defun linha (y tabuleiro)
    (nth y tabuleiro)
)

(defun celula (y x tabuleiro)
    (nth x (linha y tabuleiro))
)

(defun lista-numeros ( &optional (n 100) (lista '()) (num 0) )
    (cond
        ((= num n) lista)
        (T (lista-numeros n (cons num lista) (+ num 1)))
    )
)

(defun lista-duplos ( &optional (n 10) (lista '()) (num 0))
    (cond
        ((= num n) lista)
        (T (lista-duplos n (cons (+ (* num 10) num) lista) (+ num 1)))
    )
)

(defun escolher-aleatorio (lista)
    (cond
        ((= (length lista) 0) NIL)
        (T (nth (random (length lista)) lista))
    )
)

(defun baralhar (remover &optional (nova-lista '()) (num (escolher-aleatorio remover)))
    (cond 
        ((= (length remover) 0) nova-lista)
        (T (baralhar (remove-if #'(lambda (x) (= x num)) remover) (cons num nova-lista)))
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
    ((or (not x) (not y)) lista)
    ((> y (- (length lista) 1)) NIL)
    (T (append (subseq lista 0 y) 
            (cons (substituir-posicao x (linha y lista) valor) (subseq lista (+ y 1)))))
    )
)

  (defun valuep (value lista)
  (and (listp lista) (member value lista)))

(defun posicao (value lista)
  (let ((result (find value lista :test #'valuep)))
    (when result
      (list (position value result) (position result lista)))))

(defun posicao-player (lista player)
    (posicao player lista)
)

(defun simetrico (num)
    (multiple-value-bind (q r) (floor num 10) (+ (* r 10) q))
)

(defun duplop (num)
    (= num (simetrico num))
)

(defun existep (lista value)
    (and (not (first (posicao value lista))) (not (second (posicao value lista)))) 
)

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

(defun substituir-simetrico (num lista)
    (let ((duplos (remove-if #'(lambda (x) (= x num)) (lista-duplos))))
        (cond 
            ((and (duplop num) (> (length duplos) 0)) (substituir-duplo-max (reverse duplos) lista))
            ((not (existep lista (simetrico num))) lista)
            (T (substituir (first (posicao (simetrico num) lista)) (second (posicao (simetrico num) lista)) lista))
        )
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
        ((not (first pos)) NIL)
        ((not (second pos)) NIL)
        ((or (< (first pos) 0) (< (second pos) 0)) NIL)
        ((or (> (first pos) 9) (> (second pos) 9)) NIL)
        ((not (celula (second pos) (first pos) lista)) NIL)
        (T T)
    )
)

(defun valor-posicao (lista pos)
    (cond
        ((not lista) NIL)
        ((not pos) NIL)
        (T (celula (second pos) (first pos) lista))
    )
)

;;(defun posicoes-iniciais (lista)
;;    (let ((pos-possiveis (lista-numeros 10)))
;;        (remove-if #'(lambda (x) (not (posicao-valida lista (list x 0)))) pos-possiveis)
;;    )
;;)

(defun jogada (tabuleiro pos player)
        (substituir (first pos) (second pos) 
        (substituir-simetrico 
            (celula (second pos) (first pos) tabuleiro) 
            (substituir (first pos) (second pos) tabuleiro NIL))
    player)
)

(defun operador (tabuleiro coluna linha player)
    (let (
            (pos (if (posicao-player tabuleiro player) (posicao-player tabuleiro player) (list coluna linha)))
        )
        (cond
            ((equal pos (list coluna linha)) (jogada tabuleiro pos player))
            ((posicao-valida tabuleiro (nova-posicao pos coluna linha)) (jogada tabuleiro (nova-posicao pos coluna linha) player))
            (T NIL)
        )
    )   
)

(defun operadores-validos (operadores tabuleiro player)
    (remove-if #'(lambda (x) (not (funcall x tabuleiro player))) operadores)
)

(defun operadores ()
    (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8)
)

(defun operador-1 (tabuleiro player)
    (operador tabuleiro -1 2 player)
)

(defun operador-2 (tabuleiro player)
    (operador tabuleiro 1 2 player)
)

(defun operador-3 (tabuleiro player)
    (operador tabuleiro 2 1 player)
)

(defun operador-4 (tabuleiro player)
    (operador tabuleiro 2 -1 player)
)

(defun operador-5 (tabuleiro player)
    (operador tabuleiro 1 -2 player)
)

(defun operador-6 (tabuleiro player)
    (operador tabuleiro -1 -2 player)
)

(defun operador-7 (tabuleiro player)
    (operador tabuleiro -2 -1 player)
)

(defun operador-8 (tabuleiro player)
    (operador tabuleiro -2 1 player)
)

(defun operador-inicial (tabuleiro coluna player)
    (operador   tabuleiro 
                coluna
                (player-starting-line player)
                player)
)

(defun numero-letra (letra)
    (- (char-code (coerce letra 'character)) (char-code #\A))
)

(defun game-overp (operadores)
    (NULL operadores)
)

(defun troca-player (player)
    (- (- player) 3)
)

(defun player-starting-line (player)
    (- (* (- player) 9) 9)
)

(defun jogo (player tabuleiro &optional (operadores (operadores)))
    (let ((operadores-validos (operadores-validos operadores tabuleiro player)))
        (cond ((game-overp operadores-validos) (troca-player player))
            (T (progn
                (print-tabuleiro tabuleiro)
                (print (format nil "Player ~a~%" player))
                (jogo (troca-player player) 
                    (funcall (ler-operador operadores-validos) tabuleiro player) 
                    operadores)
            ))
        )
    )
)
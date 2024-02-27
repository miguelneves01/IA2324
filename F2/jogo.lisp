(defun linha (y tabuleiro)
    (nth y tabuleiro)
)

(defun celula (x y tabuleiro)
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
    (and (first (posicao value lista)) (second (posicao value lista))) 
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

(defun nova-posicao-pos-operador (tabuleiro operador player)
    (posicao-player (funcall operador tabuleiro player) player)
)

(defun posicao-valida (lista pos)
    (cond
        ((not lista) NIL)
        ((not pos) NIL)
        ((not (first pos)) NIL)
        ((not (second pos)) NIL)
        ((or (< (first pos) 0) (< (second pos) 0)) NIL)
        ((or (> (first pos) 9) (> (second pos) 9)) NIL)
        ((not (celula (first pos) (second pos) lista)) NIL)
        (T T)
    )
)

(defun valor-posicao (lista pos)
    (cond
        ((not lista) NIL)
        ((not pos) NIL)
        (T (celula (first pos) (second pos) lista))
    )
)

(defun jogada (tabuleiro pos nova-pos player)
        (substituir (first nova-pos) (second nova-pos) 
        (substituir-simetrico 
            (celula (first nova-pos) (second nova-pos) tabuleiro) 
            (substituir (first pos) (second pos) tabuleiro NIL))
    player)
)

(defun operador (tabuleiro coluna linha player)
    (let (
            (pos (posicao-player tabuleiro player))
        )
            (if (posicao-valida tabuleiro (nova-posicao pos coluna linha)) (jogada tabuleiro pos (nova-posicao pos coluna linha) player) NIL)
    )   
)

(defun operadores-validos (operadores tabuleiro player &optional 
                                      (posicoes (posicoes-validas operadores tabuleiro player (posicoes-validas operadores tabuleiro (troca-player player)))))
    (remove-if #'(lambda (x) (or (not (funcall x tabuleiro player)) 
                                 (not (member (posicao-player (funcall x tabuleiro player) player) posicoes :test #'equal)))
                ) operadores
    )
)

(defun posicoes-validas (operadores tabuleiro player &optional (posicoes '()))
    (remove-if #'(lambda (x) (or (not x) (member x posicoes :test #'equal))) 
        (mapcar #'(lambda (op) (nova-posicao-pos-operador tabuleiro op player)) operadores))
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
    (substituir coluna (player-starting-line player) 
        (substituir-simetrico 
            (celula coluna (player-starting-line player) tabuleiro) 
            tabuleiro)
    player)
)

(defun letra-numero (letra)
    (- (char-code (coerce letra 'character)) (char-code #\A))
)

(defun numero-letra (numero)
    (code-char (+ numero (char-code #\A)))
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

(defun jogada-inicial (tabuleiro &optional (player -1) (scores '(0 0)) (count 0))
    (cond
        ((= count 2) (list tabuleiro player scores))
        (T
            (print-tabuleiro tabuleiro)
            (format t "Jogador ~d escolha a coluna (A-J):" (- player))
            (let ((novo-tabuleiro (operador-inicial tabuleiro (letra-numero (read)) player)))
            (jogada-inicial 
                novo-tabuleiro 
                (troca-player player) 
                (add-score-to-player player scores (valor-posicao tabuleiro (posicao-player novo-tabuleiro player)))
                (+ count 1))
            )
        )
    )
)
(defun jogada-inicial-player-ai (tabuleiro &optional (ai -2) (player -1) (scores '(0 0)) (count 0))
    (cond
        ((= count 2) (list tabuleiro player scores))
        (T 
            (progn
                (print-tabuleiro tabuleiro)
                (format t "Jogador ~d escolha a coluna (A-J):" (- player))
                ( if (equal player ai)
                    (let ((novo-tabuleiro (operador-inicial tabuleiro (escolher-aleatorio (lista-numeros 10)) player)))
                        (jogada-inicial-player-ai 
                            novo-tabuleiro
                            ai 
                            (troca-player player)
                            (add-score-to-player player scores (valor-posicao tabuleiro (posicao-player novo-tabuleiro player)))
                            (+ count 1))
                    )
                    (let ((novo-tabuleiro (operador-inicial tabuleiro (letra-numero (read)) player)))
                            (jogada-inicial-player-ai 
                                novo-tabuleiro 
                                ai 
                                (troca-player player)
                                (add-score-to-player player scores (valor-posicao tabuleiro (posicao-player novo-tabuleiro player)))
                                (+ count 1)
                            )
                    )
                )
            )
        )
    )
)

(defun player-player (no &optional (tabuleiro (no-tabuleiro no)) (player (no-player no)) (pontos (no-score no)) (operadores (operadores)))
    (let ((operadores-validos (operadores-validos operadores tabuleiro player)))
        (cond ((game-overp operadores-validos) (no-score no))
            (T (progn
                (print-tabuleiro tabuleiro)
                (format t "Player ~a (~a) to play!~%Score: P1-~d : ~d-P2~%" (- player) (print-posicao (posicao-player tabuleiro player)) (first pontos) (second pontos))
                (let ((novo-tabuleiro (funcall (ler-operador operadores-validos (posicoes-validas operadores-validos tabuleiro player)) tabuleiro player)))
                (player-player
                    (list
                            novo-tabuleiro 
                            (troca-player player) 
                            (add-score-to-player player pontos (valor-posicao tabuleiro (posicao-player novo-tabuleiro player)))
                    )
                )                  
                ))
            )
        )
    )
)


(defun player-only (tabuleiro)
    (player-player (jogada-inicial tabuleiro))
)

(defun player-ai (tabuleiro)
    (player-ai-game (jogada-inicial-player-ai tabuleiro -2) -2)
)

(defun ai-player (tabuleiro)
    (player-ai-game (jogada-inicial-player-ai  tabuleiro -1) -1)
)

(defun player-ai-game (no &optional (ai -2) (tabuleiro (first no)) (player (second no)) (pontos (third no)) (operadores (operadores)))
    (let ((operadores-validos (operadores-validos operadores tabuleiro player)))
        (cond ((game-overp operadores-validos) pontos)
            (T 
                (print-tabuleiro tabuleiro)
                (format t "Player ~a (~a) to play!~%Score: P1-~d : ~d-P2~%" (- player) (print-posicao (posicao-player tabuleiro player)) (first pontos) (second pontos))
                (let ((novo-tabuleiro (if (equal player ai) 
                                      (funcall (escolher-aleatorio (operadores-validos (operadores) tabuleiro player)) tabuleiro player)
                                      (funcall (ler-operador operadores-validos (posicoes-validas operadores-validos tabuleiro player)) tabuleiro player)
                                )
                    ))
                    (player-ai-game
                        (list 
                            novo-tabuleiro 
                            (troca-player player) 
                            (add-score-to-player player pontos (valor-posicao tabuleiro (posicao-player novo-tabuleiro player)))
                        )
                        ai
                    )
                )                  
            )
        )
    )
)
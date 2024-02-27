(defun criar-no (tabuleiro player score &optional (profundidade 0) (operadores-utilizados '()))
    (list tabuleiro player score profundidade operadores-utilizados)
)

(defun no-player (no)
    (second no)
)

(defun no-tabuleiro (no)
    (first no)
)

(defun no-score (no)
    (third no)
)

(defun no-profundidade (no)
    (fourth no)
)

(defun no-operadores-utilizados (no)
    (fifth no)
)

(defun no-melhor-jogada (no)
    (first (no-operadores-utilizados no))
)

(defun value (score player)
    (float (/ (nth (- player) score) (nth (troca-player player) score)))
)

(defun add-score-to-player (player pontos score)
    (if (equal player -1)
        (list (+ (first pontos) score) (second pontos))
        (list (first pontos) (+ (second pontos) score))
    )
)

(defun novo-sucessor (no operador &optional (player (no-player no)) (novo-tabuleiro (funcall operador (no-tabuleiro no) player)))
    (criar-no
        novo-tabuleiro 
        (troca-player player)
        (add-score-to-player player (no-score no) (valor-posicao (no-tabuleiro no) (posicao-player novo-tabuleiro player)))
        (+ 1 (no-profundidade no))
        (append (no-operadores-utilizados no) (list operador))
    )
)

(defun sucessores (no &optional (operadores (operadores)))
    (mapcar (lambda (operador) (novo-sucessor no (no-player no) operador)) (operadores-validos operadores (no-tabuleiro no) (no-player no)))
)

(defun nos-iniciais (tabuleiro score player)
    (sucessores (criar-no tabuleiro player score))
)

(defun minimax ()

)
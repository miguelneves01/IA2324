(defun criar-no (tabuleiro player score &optional (profundidade 0) (maximizing T) (operadores-utilizados '()))
    (list tabuleiro player score profundidade
          (if maximizing (evaluate-scores score (troca-player player)) (evaluate-scores score player))
           operadores-utilizados)
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

(defun no-value (no)
    (fifth no)
)

(defun no-operadores-utilizados (no)
    (sixth no)
)

(defun no-melhor-jogada (no)
    (first (no-operadores-utilizados no))
)

(defun add-score-to-player (player pontos score)
    (if (equal player -1)
        (list (+ (first pontos) score) (second pontos))
        (list (first pontos) (+ (second pontos) score))
    )
)

(defun no-inicial (no)
    (criar-no (no-tabuleiro no) (no-player no) (no-score no))    
)

(defun novo-sucessor (no operador maximizing &optional (player (no-player no)) (novo-tabuleiro (funcall operador (no-tabuleiro no) player)))
    (criar-no
        novo-tabuleiro 
        (troca-player player)
        (add-score-to-player player (no-score no) (valor-posicao (no-tabuleiro no) (posicao-player novo-tabuleiro player)))
        (+ 1 (no-profundidade no))
        maximizing
        (append (no-operadores-utilizados no) (list operador))
    )
)

(defun sucessores (no maximizing &optional (operadores (operadores)))
    (mapcar #'(lambda (operador) (novo-sucessor no operador maximizing)) (operadores-validos operadores (no-tabuleiro no) (no-player no)))
)

(defun check-alpha (alpha val)
    (max alpha val)
)

(defun check-beta (beta val)
    (min beta val)
)

(defun max-no (nos &optional (max (apply #'max (mapcar #'no-value nos))))
        (car (remove-if #'(lambda (x) (not (equal (funcall 'no-value x) max))) nos))
)

(defun min-no (nos &optional (min (apply #'min (mapcar #'no-value nos))))
        (car (remove-if #'(lambda (x) (not (equal (funcall 'no-value x) min))) nos))
)

(defun evaluate-scores (score player)
    (- (nth (- (- player) 1) score) (nth (- (- (troca-player player)) 1) score))
)

(defun alphabeta (no maximizing prof-max tempo &optional (alpha -99999) (beta +99999))
    (cond 
        ((or (game-overp (no-tabuleiro no))
          (>= (no-profundidade no) prof-max)
          (>= alpha beta)
          (>= (get-internal-real-time) tempo)) no)
        (T 
            (let (( sucess 
                    (mapcar #'(lambda (x) (alphabeta x (not maximizing) prof-max tempo (check-alpha alpha (no-value x)) (check-beta beta (no-value x)))) 
                        (sucessores no maximizing))))
                (if maximizing
                    (max-no sucess)
                    (min-no sucess)
                )
            )
        )
    )
)
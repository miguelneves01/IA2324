(defun criar-no (tabuleiro &optional (profundidade 0) (pontuacao 0) (operadores-utilizados '()))
    (list tabuleiro profundidade pontuacao operadores-utilizados)
)

(defun nos-iniciais (tabuleiro)
    (mapcar #'(lambda (x) 
    (criar-no 
        (operador-inicial tabuleiro x)
        1
        (valor-posicao tabuleiro (list x 0))
        (list 'operador-inicial (list x 0))
    )) (posicoes-iniciais tabuleiro))
)

(defun no-tabuleiro (no)
    (car no)
)

(defun no-profundidade (no)
    (car (cdr no))
)

(defun no-pontuacao (no)
    (car (cdr (cdr no)))
)

(defun no-operadores-utilizados (no)
    (car (cdr (cdr (cdr no))))
)

(defun no-adicionar-operador-utilizado (no operador)
    (append (no-operadores-utilizados no) (list operador))
)

(defun novo-sucessor (no operador)
    (cond 
        ((not (funcall operador (no-tabuleiro no))) nil)
        (T (list (funcall operador (no-tabuleiro no)) (+ (no-profundidade no) 1) 
        (+ (valor-posicao (no-tabuleiro no) (posicao-cavalo (funcall operador (no-tabuleiro no)))) (no-pontuacao no)) 
        (no-adicionar-operador-utilizado no operador)))
    )
)

(defun sucessores (no operadores lista)
  (remove-if #'(lambda (x) (or (not x))) (mapcar #'(lambda (x) (novo-sucessor no x)) operadores))
)

(defun solucaop (no objetivo)
    (>= (no-pontuacao no) objetivo)
)

(defun no-existep (no lista)
    (cond 
        ((null lista) NIL)
        ((and (equal (no-tabuleiro no) (no-tabuleiro (car lista))) (> (no-profundidade no) (no-profundidade (car lista)))) T)
        (T (no-existep no (cdr lista)))
    )
)

(defun abertos-dfs (nos-abertos nos-sucessores)
    (append nos-sucessores nos-abertos)
)

(defun dfs2 (no abertos objetivo &optional (fechados '()) (solucao no) (operadores (operadores)))
    (cond 
        ((null abertos) "solucao nao encontrada")
        ((solucaop no objetivo) no)
        ((OR (no-existep no fechados) (game-overp no operadores))
            (dfs2 
                (car abertos) 
                (cdr abertos) 
                objetivo
                fechados
                operadores 
            )
        )
        (T (dfs2 
                (car (abertos-dfs abertos (sucessores no (operadores-validos operadores (no-tabuleiro no)) abertos))) 
                (cdr (abertos-dfs abertos (sucessores no (operadores-validos operadores (no-tabuleiro no)) abertos)))
                objetivo 
                (cons no fechados)  
                operadores 
            )
        )
    )
)

(defun dfs (tabuleiro objetivo)
    (dfs2 (car (nos-iniciais tabuleiro)) (cdr (nos-iniciais tabuleiro)) objetivo)
)

(defun abertos-bfs (nos-abertos nos-sucessores)
    (append nos-abertos nos-sucessores)
)

(defun bfs2 (no abertos objetivo &optional (fechados '()) (operadores (operadores)))
    (cond 
        ((null abertos) "solucao nao encontrada")
        ((solucaop no objetivo) no)
        ((OR (no-existep no fechados) (game-overp no operadores))
            (bfs2 
                (car abertos) 
                (cdr abertos) 
                objetivo
                fechados
                operadores 
            )
        )
        (T (bfs2 
                (car abertos) 
                (cdr (abertos-bfs abertos (sucessores no (operadores-validos operadores (no-tabuleiro no)) abertos)))
                objetivo 
                (cons no fechados)  
                operadores 
            )
        )
    )
)

(defun bfs (tabuleiro objetivo)
    (bfs2 (car (nos-iniciais tabuleiro)) (cdr (nos-iniciais tabuleiro)) objetivo)
)


(defun criar-no-a-star (tabuleiro &optional (profundidade 0) (pontuacao 0) (operadores-utilizados '()) (heuristica 0) (valor-total (valor-tabuleiro tabuleiro)) (valor-ultima-jogada 0))
    (list tabuleiro profundidade pontuacao operadores-utilizados heuristica valor-total valor-ultima-jogada)
)

(defun nos-iniciais-a-star (tabuleiro heuristicaf)
    (mapcar #'(lambda (x) 
    (criar-no 
        (operador-inicial tabuleiro x)
        1
        (valor-posicao tabuleiro (list x 0))
        (list 'operador-inicial (list x 0))
        (funcall heuristicaf  (valor-posicao tabuleiro (list x 0)) (valor-tabuleiro tabuleiro))
        valor-total
        (valor-posicao tabuleiro (list x 0))
    )) (posicoes-iniciais tabuleiro))
)

(defun no-heuristica (no)
    (car (cdr (cdr (cdr (cdr no)))))
)

(defun novo-sucessor-a-star (no operador heuristicaf)
    (cond 
        ((not (funcall operador (no-tabuleiro no))) nil)
        (T (list (funcall operador (no-tabuleiro no)) (+ (no-profundidade no) 1) 
        (+ (valor-posicao (no-tabuleiro no) (posicao-cavalo (funcall operador (no-tabuleiro no)))) (no-pontuacao no)) 
        (no-adicionar-operador-utilizado no operador)
        (funcall heuristicaf (valor-posicao (no-tabuleiro no) (posicao-cavalo (funcall operador (no-tabuleiro no)))) (valor-tabuleiro (no-tabuleiro no)))
        ))
    )
)

(defun sucessores-a-star (no operadores lista)
  (remove-if #'(lambda (x) (or (not x))) (mapcar #'(lambda (x) (novo-sucessor no x)) operadores))
)

(defun sort-a-star (nos)
    (sort nos #'> :key #'no-heuristica)
)

(defun abertos-a-star (nos-abertos nos-sucessores heuristicaf)
    (sort-a-star (append nos-abertos nos-sucessores) heuristicaf)
)

(defun a-star2 (no abertos objetivo heuristicaf &optional (fechados '()) (operadores (operadores)))
    (cond 
        ((null abertos) "solucao nao encontrada")
        ((solucaop no objetivo) no)
        ((OR (no-existep no fechados) (game-overp no operadores))
            (a-star2
                (car abertos) 
                (cdr abertos) 
                objetivo
                heuristicaf
                fechados
                operadores 
            )
        )
        (T (a-star2 
                (car abertos) 
                (cdr (abertos-a-star abertos (sucessores no (operadores-validos operadores (no-tabuleiro no)) abertos) heuristicaf))
                objetivo 
                heuristicaf
                (cons no fechados)  
                operadores 
            )
        )
    )
)

(defun a-star (tabuleiro objetivo heuristicaf)
    (a-star2 (car (nos-iniciais tabuleiro)) (cdr (nos-iniciais tabuleiro)) objetivo heuristicaf)
)
;;(tabuleiro profundidade pontuação no-pai operadores-utilizados)

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
  (remover-se #'(lambda (x) (or (not x) (no-existep x lista))) (mapcar #'(lambda (x) (novo-sucessor no x)) operadores))
)

(defun abertos-bfs (nos-abertos nos-sucessores)
    (append nos-abertos nos-sucessores)
)

(defun abertos-dfs (nos-abertos nos-sucessores)
    (append nos-sucessores nos-abertos)
)

(defun game-overp (no operadores)
    (cond 
        ((NULL (operadores-validos operadores (no-tabuleiro no))) T)
        (T NIL)
    )
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

(defun dfs2 (no abertos objetivo &optional (fechados '()) (solucao no) (operadores (operadores)))
    (cond 
        ((null abertos) "solucao nao encontrada")
        ((no-existep no fechados) 
            (dfs2 
                (car abertos) 
                (cdr abertos) 
                objetivo
                (cons no fechados)
                operadores 
            )
        )
        ((solucaop no objetivo) no)
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

(defun bfs2 (no abertos objetivo &optional (fechados '()) (operadores (operadores)))
    (cond 
        ((null abertos) "solucao nao encontrada")
        ((no-existep no fechados) 
            (bfs2 
                (car abertos) 
                (cdr abertos) 
                objetivo
                (cons no fechados)
                operadores 
            )
        )
        ((solucaop no objetivo) no)
        (T (bfs2 
                (car (abertos-bfs abertos (sucessores no (operadores-validos operadores (no-tabuleiro no)) abertos))) 
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
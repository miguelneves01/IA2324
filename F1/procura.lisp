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

(defun sucessores (no operadores &optional (prof-maxima 8))
    (cond ((= (no-profundidade no) prof-maxima) NIL)
          (T (remover-se #'(lambda (x) (not x)) (mapcar #'(lambda (x) (novo-sucessor no x)) operadores)))
    )
)

(defun abertos-bfs (nos-abertos nos-sucessores)
    (reduce #'cons nos-abertos :initial-value nos-sucessores :from-end t)
)

(defun abertos-dfs (nos-abertos nos-sucessores)
    (stable-sort (reduce #'cons nos-abertos :initial-value nos-sucessores :from-end t) 
                 #'(lambda (x y) (> (no-profundidade x) (no-profundidade y)))
    )
)

(defun dfs (tabuleiro)
    (dfs2 (car (nos-iniciais tabuleiro)) (cdr (nos-iniciais tabuleiro)))
)

(defun dfs2 (no &optional (abertos (nos-iniciais (no-tabuleiro no))) (fechados ()) (solucao no) (operadores (operadores))  (max-prof 8))
  (let* ((novos-abertos (abertos-dfs abertos (sucessores no (operadores-validos operadores (no-tabuleiro no)) max-prof))))
     (cond 
         ((null novos-abertos) solucao)
         ((OR (> (no-profundidade no) max-prof) (no-existep no fechados)) (dfs2 (car novos-abertos) (cdr novos-abertos) (cons no fechados) solucao operadores max-prof ))
         ((and (solucaop no operadores) (> (no-pontuacao no) (no-pontuacao solucao))) (dfs2 (car novos-abertos) (cdr novos-abertos) (cons no fechados) no operadores max-prof ))
         (T (dfs2 (car novos-abertos) (cdr novos-abertos) (cons no fechados) solucao operadores max-prof ))
     )
  )
)

(defun solucaop (no operadores)
    (cond 
        ((NULL (operadores-validos operadores (no-tabuleiro no))) T)
        (T NIL)
    )
)

(defun no-existep (no lista)
    (cond 
        ((null lista) NIL)
        ((equal (no-tabuleiro no) (no-tabuleiro (car lista))) T)
        (T (no-existep no (cdr lista)))
    )
)
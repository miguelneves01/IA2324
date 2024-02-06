;;(tabuleiro profundidade pontuação no-pai operadores-utilizados)

(defun criar-no (tabuleiro &optional (profundidade 0) (pontuacao 0) (pai nil) (operadores-utilizados '()))
    (list tabuleiro profundidade pontuacao pai operadores-utilizados)
)

(defun nos-iniciais (tabuleiro)
    (mapcar #'(lambda (x) 
    (criar-no 
        (operador-inicial tabuleiro x)
        1
        (valor-posicao tabuleiro (list x 0))
        (criar-no tabuleiro)
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

(defun no-pai (no)
    (car (cdr (cdr (cdr no))))
)

(defun no-operadores-utilizados (no)
    (car (cdr (cdr (cdr (cdr no)))))
)

(defun no-adicionar-operador-utilizado (no operador)
    (append (no-operadores-utilizados no) (list operador))
)

(defun novo-sucessor (no operador)
    (cond 
        ((not (funcall operador (no-tabuleiro no))) nil)
        (T (list (funcall operador (no-tabuleiro no)) (+ (no-profundidade no) 1) 
        (+ (valor-posicao (no-tabuleiro no) (posicao-cavalo (funcall operador (no-tabuleiro no)))) (no-pontuacao no)) 
        no (no-adicionar-operador-utilizado no operador)))
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

(defun dfs (no solucaop nos-sucessores operadores max-prof &optional (abertos (nos-iniciais (no-tabuleiro no))) (fechados ()))
  (let* ((novos-sucessores (nos-sucessores no operadores 'dfs max-prof)))
     (cond 
         ((funcall solucaop no) no)
         ((OR (> (no-profundidade no) max-prof) (no-existep no novos-sucessores 'dfs)) nil)
         (T     
             (dfs (car novos-sucessores) solucaop nos-sucessores operadores max-prof (abertos-dfs ) (cons no fechados))
         )
     )
  )
)

(defun solucaop (no)

)

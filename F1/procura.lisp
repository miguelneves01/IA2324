#|
    Unidade Curricular de Inteligência Artificial
        Docentes:
        Prof. Joaquim Filipe
        Eng. Filipe Mariano

    Jogo do Cavalo
    Projeto realizado por Grupo 51:
        Miguel Neves - 201900377
        Guilherme Ravasco - 201900646
 |#



;; Criaçao de Nós

(defun criar-no (tabuleiro &optional (profundidade 0) (pontuacao 0) (operadores-utilizados '()))
    (list tabuleiro profundidade pontuacao operadores-utilizados)
)

(defun criar-no-a-star (tabuleiro objetivo heuristicaf profundidade pontuacao operadores-utilizados valor-tabuleiro-pai valor-ultima-jogada)
    (list 
        tabuleiro 
        profundidade 
        (+ pontuacao valor-ultima-jogada)
        operadores-utilizados
        (funcall heuristicaf 
            (valor-heuristica heuristicaf valor-ultima-jogada objetivo pontuacao)
            (valor-heuristica-2 heuristicaf (- valor-tabuleiro-pai valor-ultima-jogada) (count-numbers tabuleiro))
        )
        (- valor-tabuleiro-pai valor-ultima-jogada)
    )
)

;; Geraçao dos Nós Iniciais

(defun nos-iniciais (tabuleiro)
    (mapcar #'(lambda (x) 
    (criar-no 
        (operador-inicial tabuleiro x)
        1
        (valor-posicao tabuleiro (list x 0))
        (list 'operador-inicial (list x 0))
    )) (posicoes-iniciais tabuleiro))
)

(defun nos-iniciais-a-star (tabuleiro objetivo heuristicaf)
    (mapcar #'(lambda (x) 
    (criar-no-a-star 
        (operador-inicial tabuleiro x)
        objetivo
        heuristicaf
        1
        0
        (list 'operador-inicial (list x 0))
        (valor-tabuleiro tabuleiro)
        (valor-posicao tabuleiro (list x 0))
    )) (posicoes-iniciais tabuleiro))
)

;; Geracao de Sucessores

(defun novo-sucessor (no operador)
    (cond 
        ((not (funcall operador (no-tabuleiro no))) nil)
        (T 
            (criar-no 
                (funcall operador (no-tabuleiro no)) 
                (+ (no-profundidade no) 1) 
                (+ (valor-posicao-jogada no operador) (no-pontuacao no)) 
                (no-adicionar-operador-utilizado no operador))
         )
    )
)

(defun sucessores (no operadores)
  (remove-if #'(lambda (x) (or (not x))) (mapcar #'(lambda (x) (novo-sucessor no x)) operadores))
)

(defun novo-sucessor-a-star (no objetivo operador heuristicaf)
    (cond 
        ((not (funcall operador (no-tabuleiro no))) nil)
        (T 
            (criar-no-a-star
                (funcall operador (no-tabuleiro no))
                objetivo
                heuristicaf
                (+ (no-profundidade no) 1) 
                (no-pontuacao no)
                (no-adicionar-operador-utilizado no operador)
                (no-pontuacao-disponivel no)
                (valor-posicao-jogada no operador)            
            )
        )
     )
)


(defun sucessores-a-star (no objetivo operadores heuristicaf)
  (remove-if #'(lambda (x) (or (not x))) (mapcar #'(lambda (x) (novo-sucessor-a-star no objetivo x heuristicaf)) operadores))
)

;; Seletores do Nó

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

(defun no-heuristica (no)
    (car (cdr (cdr (cdr (cdr no)))))
)

(defun no-pontuacao-disponivel (no)
    (car (cdr (cdr (cdr (cdr (cdr no))))))
)

;; Funcoes auxiliares e predicados

(defun no-adicionar-operador-utilizado (no operador)
    (append (no-operadores-utilizados no) (list operador))
)

(defun valor-posicao-jogada (no operador)
    (valor-posicao (no-tabuleiro no) (posicao-cavalo (funcall operador (no-tabuleiro no))))
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

(defun game-overp (no operadores)
    (cond 
        ((NULL (operadores-validos operadores (no-tabuleiro no))) T)
        (T NIL)
    )
)

;; Ordenacao DFS

(defun abertos-dfs (nos-abertos nos-sucessores)
    (append nos-sucessores nos-abertos)
)

;; Ordenacao BFS

(defun abertos-bfs (nos-abertos nos-sucessores)
    (append nos-abertos nos-sucessores)
)

;; Ordenacao A*

(defun sort-a-star (nos)
    (sort nos #'> :key #'no-heuristica)
)

(defun abertos-a-star (nos-abertos nos-sucessores)
    (sort-a-star (append nos-abertos nos-sucessores))
)

;; Heuristicas A*

(defun heuristica-maiorvalor (valor valor-total)
    (cond 
        ((or (not valor) (not valor-total)) 0)
        (T (float (/ valor (+ valor-total valor))))
    )
)

(defun heuristica-dada (valor valor-total)
    (cond 
        ((or (not valor) (not valor-total)) 0)
        (T (float (/ valor valor-total)))
    )
)

(defun valor-heuristica (heuristicaf valor-ultima-jogada objetivo pontuacao)
    (cond 
        ((equal heuristicaf 'heuristica-maiorvalor) valor-ultima-jogada)
        ((equal heuristicaf  'heuristica-dada) (- objetivo (+ pontuacao valor-ultima-jogada)))
    )
)

(defun valor-heuristica-2 (heuristicaf valor-atual-pai qtty-numbers)
    (cond 
        ((equal heuristicaf 'heuristica-maiorvalor) valor-atual-pai)
        ((equal heuristicaf  'heuristica-dada) (/ valor-atual-pai qtty-numbers))
    )
)

;; A*

(defun a-star2 (no abertos objetivo heuristicaf &optional (fechados '()) (operadores (operadores)) (ramificacoes (list (length abertos))))
    (cond 
        ((solucaop no objetivo) (list no (+ (length fechados) (length abertos)) (length fechados) (float (/ (apply #'+ ramificacoes) (length ramificacoes)))))
        ((null abertos) nil)
        ((OR (no-existep no fechados) (game-overp no operadores))
            (a-star2
                (car abertos) 
                (cdr abertos) 
                objetivo
                heuristicaf
                (cons no fechados)
                operadores
                ramificacoes
            )
        )
        (T 
            (let ((novos-nos (sucessores-a-star no objetivo (operadores-validos operadores (no-tabuleiro no)) heuristicaf)))
                (a-star2 
                    (car (abertos-a-star abertos novos-nos)) 
                    (cdr (abertos-a-star abertos novos-nos))
                    objetivo 
                    heuristicaf
                    (cons no fechados)  
                    operadores
                    (cons (length novos-nos) ramificacoes)
                )
            )
        )
    )
)

(defun a-star (tabuleiro objetivo heuristicaf)
    (let ((iniciais (nos-iniciais-a-star tabuleiro objetivo heuristicaf)))
        (let ((solucao (time (a-star2 (car iniciais) (cdr iniciais) objetivo heuristicaf))))
            (cond 
                ((not solucao) "~%Solucao nao encontrada ~%")
                (T 
                (format t "~%Profundidade: ~d"  (no-profundidade (first solucao)))
                (format t "~%Nos Gerados: ~d"  (second solucao))
                (format t "~%Nos Expandidos: ~d" (third solucao))
                (format t "~%Fator de Ramificacao Medio: ~d" (fourth solucao))
                (format t "~%Penetrancia: ~d" (float (/ (no-profundidade (first solucao)) (second solucao))))
                (format t "~%Pontuacao: ~d" (no-pontuacao (first solucao)))
                (format t "~%Solucao: ~d" (no-operadores-utilizados (first solucao)))
                (format t "~%Heuristica: ~d ~%" (no-heuristica (first solucao)))
                (print-tabuleiro (no-tabuleiro (first solucao)))
                )
            )
        )
    )
)

;; Algoritmo Geral

(defun algoritmo (sorting no abertos objetivo &optional (fechados '()) (operadores (operadores)) (ramificacoes (list (length abertos))))
    (cond 
        ((solucaop no objetivo) (list no (+ (length fechados) (length abertos)) (length fechados) (float (/ (apply #'+ ramificacoes) (length ramificacoes)))))
        ((null abertos) nil)
        ((OR (no-existep no fechados) (game-overp no operadores))
            (algoritmo
                sorting
                (car abertos) 
                (cdr abertos) 
                objetivo
                fechados
                operadores
                ramificacoes
            )
        )
        (T 
         (let ((new-abertos (sucessores no (operadores-validos operadores (no-tabuleiro no)))))
            (algoritmo
                sorting
                (car (funcall sorting abertos new-abertos)) 
                (cdr (funcall sorting abertos new-abertos))
                objetivo 
                (cons no fechados)  
                operadores
                (cons (length new-abertos) ramificacoes)
            )
         )
        )
    )
)

(defun call-algoritmo (sorting tabuleiro objetivo)
    (let ((iniciais (nos-iniciais tabuleiro)))
        (let ((solucao (time (algoritmo sorting (car iniciais) (cdr iniciais) objetivo))))
            (cond 
                ((not solucao) "~%Solucao nao encontrada ~%")
                (T 
                (format t "~%Profundidade: ~d"  (no-profundidade (first solucao)))
                (format t "~%Nos Gerados: ~d"  (second solucao))
                (format t "~%Nos Expandidos: ~d" (third solucao))
                (format t "~%Fator de Ramificacao Medio: ~d" (fourth solucao))
                (format t "~%Penetrancia: ~d" (float (/ (no-profundidade (first solucao)) (second solucao))))
                (format t "~%Pontuacao: ~d~%" (no-pontuacao (first solucao)))
                (format t "~%Solucao: ~d" (no-operadores-utilizados (first solucao)))
                (print-tabuleiro (no-tabuleiro (first solucao)))
                )
            )
        )
    )
)
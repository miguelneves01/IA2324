;;(load "//wsl.localhost/Ubuntu-22.04/home/miguel/dev/IA2324/F1/puzzle.lisp")
(load "C:/IPS/EI/3Ano/IA/Projeto/F1/puzzle.lisp")
;;(load "//wsl.localhost/Ubuntu-22.04/home/miguel/dev/IA2324/F1/procura.lisp")
(load "C:/IPS/EI/3Ano/IA/Projeto/F1/procura.lisp")

(defun carregar-tabuleiros 
    (filepath)
    (with-open-file 
        (file filepath :direction :input)
        (loop for expression = 
            (read file nil nil)
            while expression
            collect expression
        )
    )
)

(defun escolher-tabuleiro 
    (lists)
    (format t "~%Escolhe o tabuleiro:~%")
    (loop for i from 1 to 
        (length lists) do
        (format t "~d. ~%" i)
    )
    (nth (- (read) 1) lists)
)

(defun ler-tabuleiro ()
    (format t "Usar tabuleiro aleatorio? (S/N) ~%")
    (case (read)
        ('s (tabuleiro-aleatorio))
        ('n (escolher-tabuleiro (carregar-tabuleiros  "C:/IPS/EI/3Ano/IA/Projeto/F1/problemas.dat")))
    )
)

(defun ler-algoritmo ()
    (format t "Escolhe o algoritmo:~%1. DFS~%2. BFS~%3. A*~%")
    (case (read)
        (1 'dfs)
        (2 'bfs)
        (3 'a-star)
    )
)

(defun ler-objetivo ()
    (format t "Qual eh o objetivo?~%")
    (read)
)

(defun ler-heuristica ()
    (format t "Escolhe a heuristica:~%1. Maior Valor~%")
    (case (read)
        (1 'no-heuristica-1)
        ;;(2 'heuristica-2)
    )
)

(defun init ()
    (let ((algoritmo (ler-algoritmo)))
        (case algoritmo
            ('a-star (a-star (ler-tabuleiro) (ler-objetivo) (ler-heuristica)))
            ('dfs (dfs (ler-tabuleiro) (ler-objetivo)))
            ('bfs (bfs (ler-tabuleiro) (ler-objetivo)))
        )
    )
)
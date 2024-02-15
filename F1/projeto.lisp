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
    (cond
        ((eql (read) 'S) 
            (tabuleiro-aleatorio)
        )
        ;;( T (escolher-tabuleiro (carregar-tabuleiros  "//wsl.localhost/Ubuntu-22.04/home/miguel/dev/IA2324/F1/problemas.dat")))
        ( T (escolher-tabuleiro (carregar-tabuleiros  "C:/IPS/EI/3Ano/IA/Projeto/F1/problemas.dat")))
    )
)

(defun ler-algoritmo ()
    (format t "Escolhe o algoritmo:~%1. DFS~%2. BFS~%")
    (case (read)
        (1 'dfs)
        (2 'bfs)
    )
)

(defun init ()
    (funcall (ler-algoritmo) (ler-tabuleiro))
)
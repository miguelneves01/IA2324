(load "//wsl.localhost/Ubuntu-22.04/home/miguel/dev/IA2324/F1/puzzle.lisp")
(load "//wsl.localhost/Ubuntu-22.04/home/miguel/dev/IA2324/F1/procura.lisp")

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
        ( T (escolher-tabuleiro (carregar-tabuleiros  "//wsl.localhost/Ubuntu-22.04/home/miguel/dev/IA2324/F1/problemas.dat")))
    )
)
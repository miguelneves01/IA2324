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

(load (merge-pathnames "puzzle.lisp" *compile-file-pathname*))
(load (merge-pathnames "procura.lisp" *compile-file-pathname*))
(defvar *problemas* (merge-pathnames "problemas.dat" *compile-file-pathname*))

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
    (tabuleiros)
    (format t "~%Escolhe o tabuleiro:~%")
    (loop for i from 1 to 
        (length tabuleiros) do
        (format t "~a.~%" (code-char (+ (char-code #\A) (- i 1))))
        (print-tabuleiro (nth (- i 1) tabuleiros))
        (format t "~%")
    )
    (nth (- (char-code (coerce (read) 'character)) (char-code #\A)) tabuleiros)
)

(defun escrever-aleatorio ()
    (with-open-file 
        (file *problemas* :direction :output :if-exists :append)
        (format file "~%~a" (tabuleiro-aleatorio))
    )
)

(defun ler-tabuleiro ()
    (escrever-aleatorio)
    (escolher-tabuleiro (carregar-tabuleiros *problemas* ))
)

(defun ler-algoritmo ()
    (format t "Escolhe o algoritmo:~%1. DFS~%2. BFS~%3. A*~%")
    (case (read)
        (1 'abertos-dfs)
        (2 'abertos-bfs)
        (3 'a-star)
    )
)

(defun ler-objetivo ()
    (format t "Qual o objetivo?~%")
    (read)
)

(defun ler-heuristica ()
    (format t "Escolhe a heuristica:~%1. Maior Valor~%2. Dada~%")
    (case (read)
        (1 'heuristica-maiorvalor)
        (2 'heuristica-dada)
    )
)

(defun print-tabuleiro (tabuleiro)
    (format t "~a~%" (first tabuleiro))
    (format t "~a~%" (second tabuleiro))    
    (format t "~a~%" (third tabuleiro))    
    (format t "~a~%" (fourth tabuleiro))    
    (format t "~a~%" (fifth tabuleiro))    
    (format t "~a~%" (sixth tabuleiro))    
    (format t "~a~%" (seventh tabuleiro))
    (format t "~a~%" (eighth tabuleiro))
    (format t "~a~%" (ninth tabuleiro))
    (format t "~a~%" (tenth tabuleiro))
)

(defun init ()
    (let ((algo (ler-algoritmo)))
        (cond 
            ((equal algo 'a-star) (a-star (ler-tabuleiro) (ler-objetivo) (ler-heuristica)))
            (T (call-algoritmo algo (ler-tabuleiro) (ler-objetivo)))
        )
    )
    
)
(load "/IPS/EI/3Ano/IA/Projeto/F1/puzzle.lisp")
(load "/IPS/EI/3Ano/IA/Projeto/F1/procura.lisp")

(defun load-lists-from-file (filepath)
    (with-open-file (file filepath :direction :input)
        (loop for expression = (read file nil nil)
            while expression
            collect expression
        )
    )
)

(defun ask-user-which-list (lists)
    (format t "~%Escolhe o tabuleiro:~%")
    (loop for i from 1 to (length lists) do
        (format t "~d. ~%" i))
    (let ((choice (read)))
        (nth (- choice 1) lists)))

(defun ler-tabuleiro ()
  ;;"/IPS/EI/3Ano/IA/Projeto/F1/problemas.dat"
(progn
  (format t "Usar tabuleiro aleatorio? (S/N) ~%")
  (let* ((rand (read)))
    (cond
     ((eql rand 'S) (tabuleiro-aleatorio))
     ( T (ask-user-which-list (load-lists-from-file "/IPS/EI/3Ano/IA/Projeto/F1/problemas.dat")))

))))
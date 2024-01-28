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
    (format t "Choose a list to use:~%")
    (loop for i from 1 to (length lists) do
        (format t "~d. " i))
    (let ((choice (read)))
        (nth (- choice 1) lists)))

(defun init (&optional (filepath "/IPS/EI/3Ano/IA/Projeto/F1/problemas.dat"))
;;"/IPS/EI/3Ano/IA/Projeto/F1/problemas.dat"
(cond
 ((not filepath) (tabuleiro-aleatorio))
 ( T
    (let ((lists (load-lists-from-file filepath)))
    (let ((chosen-list (ask-user-which-list lists)))
    (format t "You chose: ~a~%" chosen-list)))
    )
)
)
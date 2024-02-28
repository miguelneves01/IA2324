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

(load (merge-pathnames "jogo.lisp" *compile-file-pathname*))
(load (merge-pathnames "algoritmo.lisp" *compile-file-pathname*))
(defvar *log* (merge-pathnames "log.dat" *compile-file-pathname*))


(defun ler-operador (operadores-validos posicoes-validas)
    (format t "Escolha um operador:~%")
    (loop for i from 1 to (length operadores-validos) do
        (format t "~d. ~a~%" i (print-posicao (nth (- i 1) posicoes-validas)))
    )
    (nth (- (read) 1) operadores-validos)
)

(defun print-tabuleiro (tabuleiro)
    (log-message (format nil "~%    A   B   C   D   E   F   G   H   I   J~%"))
    (log-message (format nil "1  ~a~%" (print-line (first tabuleiro))))
    (log-message (format nil "2  ~a~%" (print-line (second tabuleiro))))    
    (log-message (format nil "3  ~a~%" (print-line (third tabuleiro))))  
    (log-message (format nil "4  ~a~%" (print-line (fourth tabuleiro))))    
    (log-message (format nil "5  ~a~%" (print-line (fifth tabuleiro))))    
    (log-message (format nil "6  ~a~%" (print-line (sixth tabuleiro))))    
    (log-message (format nil "7  ~a~%" (print-line (seventh tabuleiro))))
    (log-message (format nil "8  ~a~%" (print-line (eighth tabuleiro))))
    (log-message (format nil "9  ~a~%" (print-line (ninth tabuleiro))))
    (log-message (format nil "10 ~a~%~%" (print-line (tenth tabuleiro))))
)

(defun print-line (linha)
    (format nil "~3d ~3d ~3d ~3d ~3d ~3d ~3d ~3d ~3d ~3d" 
        (first linha)
        (second linha)
        (third linha)
        (fourth linha)
        (fifth linha)
        (sixth linha)
        (seventh linha)
        (eighth linha)
        (ninth linha)
        (tenth linha))
)

(defun print-posicao (posicao)
    (format nil "~a~d" (numero-letra (first posicao)) (+ (second posicao) 1))
)

(defun ler-gamemode ()
    (format t "Escolha o modo de jogo:~%1. Player vs Player~%2. Player vs AI~%3. AI vs AI~%")
    (case (read)
        (1 'player-only)
        (2 (ler-player-1))
        (3 'ai-ai)
    )
)

(defun ler-player-1 ()
    (format t "Quem comeca?~%1. Player~%2. AI~%")
    (case (read)
        (1 'player-ai)
        (2 'ai-player)
    )
)

(defun log-message (mensagem)
    (with-open-file 
        (file *log* :direction :output :if-exists :append)
        (format file mensagem)
    )
    (format t mensagem)
)

(defun init (&optional (tabuleiro (tabuleiro-aleatorio)) (tempo 1000))
    (with-open-file 
        (file *log* :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format file "Tempo: ~d ms~%~%" tempo)
    )
    (let ((resultado (funcall (ler-gamemode) tabuleiro tempo)))
        (log-message (format nil "Resultados: P1- ~d : P2- ~d~%" (first resultado) (second resultado)))
        (log-message (format nil "Vencedor: Player ~d" (if (> (first resultado) (second resultado)) 1 2)))
    )
)

(defun jogar (tabuleiro tempo &optional (player -1))
    (with-open-file 
        (file *log* :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format file "Tempo: ~d ms~%~%" tempo)
    )
    (funcall (no-melhor-jogada (alphabeta (no-inicial (list tabuleiro player '(0 0))) T 100 (+ tempo (get-internal-real-time)))) tabuleiro player)
)
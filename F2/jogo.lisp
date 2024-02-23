(defun linha (y tabuleiro)
    (nth y tabuleiro)
)

(defun celula (y x tabuleiro)
    (nth x (linha y tabuleiro))
)

(defun lista-numeros ( &optional (n 100) (lista '()) (num 0) )
    (cond
        ((= num n) lista)
        (T (lista-numeros n (cons num lista) (+ num 1)))
    )
)

(defun lista-duplos ( &optional (n 10) (lista '()) (num 0))
    (cond
        ((= num n) lista)
        (T (lista-duplos n (cons (+ (* num 10) num) lista) (+ num 1)))
    )
)

(defun escolher-aleatorio (lista)
    (cond
        ((= (length lista) 0) NIL)
        (T (nth (random (length lista)) lista))
    )
)

(defun baralhar (remover &optional (nova-lista '()) (num (escolher-aleatorio remover)))
    (cond 
        ((= (length remover) 0) nova-lista)
        (T (baralhar (remove-if #'(lambda (x) (= x num)) remover) (cons num nova-lista)))
    ) 
)

(defun tabuleiro-aleatorio (&optional (lista (baralhar(lista-numeros))) (n 10))
    (cond
        ((null lista) nil)
        (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
    )
)

(defun substituir-posicao (indice lista &optional (valor NIL))
    (cond
      ((> indice (- (length lista) 1)) NIL)
      (T (append (subseq lista 0 indice) (cons valor (subseq lista (+ indice 1)))))
    )
)

(defun substituir (x y lista &optional (valor NIL))
   (cond
    ((or (not x) (not y)) lista)
    ((> y (- (length lista) 1)) NIL)
    (T (append (subseq lista 0 y) 
            (cons (substituir-posicao x (linha y lista) valor) (subseq lista (+ y 1)))))
    )
)

  (defun valuep (value lista)
  (and (listp lista) (member value lista)))

(defun posicao (value lista)
  (let ((result (find value lista :test #'valuep)))
    (when result
      (list (position value result) (position result lista)))))

(defun posicao-player (lista player)
    (posicao player lista)
)

(defun simetrico (num)
    (multiple-value-bind (q r) (floor num 10) (+ (* r 10) q))
)

(defun duplop (num)
    (= num (simetrico num))
)

(defun existep (lista value)
    (and (not (first (posicao value lista))) (not (second (posicao value lista)))) 
)

(defun substituir-duplo-max (lista-duplos lista)
    (cond 
        ((not lista) NIL)
        ((= (length lista-duplos) 0) lista)
        ((posicao (first lista-duplos) lista) 
            (substituir 
                (first (posicao (first lista-duplos) lista))
                (second (posicao (first lista-duplos) lista))
                lista))
        (T (substituir-duplo-max (cdr lista-duplos) lista))
    )
)

(defun substituir-simetrico (num lista)
    (let ((duplos (remove-if #'(lambda (x) (= x num)) (lista-duplos))))
        (cond 
            ((and (duplop num) (> (length duplos) 0)) (substituir-duplo-max (reverse duplos) lista))
            ((not (existep lista (simetrico num))) lista)
            (T (substituir (first (posicao (simetrico num) lista)) (second (posicao (simetrico num) lista)) lista))
        )
    )
)

(defun nova-posicao (pos offset-x offset-y)
  (cond
       ((not pos) NIL)
       (T (list (+(first pos) offset-x) (+(second pos) offset-y)))
   )
)

(defun posicao-valida (lista pos)
    (cond
        ((not lista) NIL)
        ((not pos) NIL)
        ((not (first pos)) NIL)
        ((not (second pos)) NIL)
        ((or (< (first pos) 0) (< (second pos) 0)) NIL)
        ((or (> (first pos) 9) (> (second pos) 9)) NIL)
        ((not (celula (second pos) (first pos) lista)) NIL)
        (T T)
    )
)

(defun valor-posicao (lista pos)
    (cond
        ((not lista) NIL)
        ((not pos) NIL)
        (T (celula (second pos) (first pos) lista))
    )
)

;;(defun posicoes-iniciais (lista)
;;    (let ((pos-possiveis (lista-numeros 10)))
;;        (remove-if #'(lambda (x) (not (posicao-valida lista (list x 0)))) pos-possiveis)
;;    )
;;)

(defun jogada (tabuleiro pos player)
        (substituir (first pos) (second pos) 
        (substituir-simetrico 
            (celula (second pos) (first pos) lista) 
            (substituir (first pos) (second pos) lista NIL))
    player)
)

(defun operador (tabuleiro coluna linha player)
    (let (
            (pos (if (posicao-player tabuleiro) (posicao-player tabuleiro) (coluna linha)))
        )
        (cond
            ((equal pos (list coluna linha)) (jogada pos player))
            ((posicao-valida tabuleiro (nova-posicao pos coluna linha)) (jogada (nova-posicao pos coluna linha) player))
            (T NIL)
        )
    )   
)

(defun operadores-validos (operadores tabuleiro player)
    (remove-if #'(lambda (x) (not (funcall x tabuleiro player))) operadores)
)

(defun operadores ()
    (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8)
)

(defun operador-1 (tabuleiro player)
    (operador tabuleiro -1 2 player)
)

(defun operador-2 (tabuleiro player)
    (operador tabuleiro 1 2 player)
)

(defun operador-3 (tabuleiro player)
    (operador tabuleiro 2 1 player)
)

(defun operador-4 (tabuleiro player)
    (operador tabuleiro 2 -1 player)
)

(defun operador-5 (tabuleiro player)
    (operador tabuleiro 1 -2 player)
)

(defun operador-6 (tabuleiro player)
    (operador tabuleiro -1 -2 player)
)

(defun operador-7 (tabuleiro player)
    (operador tabuleiro -2 -1 player)
)

(defun operador-8 (tabuleiro player)
    (operador tabuleiro -2 1 player)
)

(defun operador-inicial (tabuleiro coluna linha player)
    (operador   tabuleiro 
                coluna
                linha
                player)
)



(defun count-numbers (tabuleiro &optional (count 0))
    (cond
        ((NULL tabuleiro) count)
        (T (count-numbers (cdr tabuleiro) (+ count (length (remove-if-not #'(lambda (x) (numberp x)) (car tabuleiro))))))
    )
)

(defun valor-tabuleiro (tabuleiro)
    (reduce #'+ (mapcar #'(lambda (x) (reduce #'+ (remove-if-not #'(lambda (y) (numberp y)) x))) tabuleiro))
)

(defun print-tabuleiro (tabuleiro)
    (format t "   A  B  C  D  E  F  G  H  I  J~%")
    (format t "1  ~a~%" (print-line (first tabuleiro)))
    (format t "2  ~a~%" (print-line (second tabuleiro)))    
    (format t "3  ~a~%" (print-line (third tabuleiro)))  
    (format t "4  ~a~%" (print-line (fourth tabuleiro)))    
    (format t "5  ~a~%" (print-line (fifth tabuleiro)))    
    (format t "6  ~a~%" (print-line (sixth tabuleiro)))    
    (format t "7  ~a~%" (print-line (seventh tabuleiro)))
    (format t "8  ~a~%" (print-line (eighth tabuleiro)))
    (format t "9  ~a~%" (print-line (ninth tabuleiro)))
    (format t "10 ~a~%" (print-line (tenth tabuleiro)))
)

(defun print-line (linha)
    (format nil "~2,'0d ~2,'0d ~2,'0d ~2,'0d ~2,'0d ~2,'0d ~2,'0d ~2,'0d ~2,'0d ~2,'0d" 
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

(defun numero-letra (letra)
    (- (char-code (coerce letra 'character)) (char-code #\A))
)

(defun game-overp (tabuleiro operadores player)
    (NULL (operadores-validos operadores tabuleiro player))
)
(defun linha (n tabuleiro)
    (nth n tabuleiro)
)

(defun celula (n m tabuleiro)
    (linha m (linha n tabuleiro))
)

(defun lista-numeros ( &optional (n 100))
    (loop for i upto (- n 1) collect i)
)

;;(remover-se #'(lambda (x) (= x 0)) '(1 2 0 2 0 4))
(defun remover-se(pred lista)
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista))))))

;;(nth (random (length lista)) lista)

(defun baralhar (lista)
    (let* ((nova-lista '()))
        (cond ((= (length lista) 0) nova-lista)
              (T (cons (nth (random (length lista)) lista) nova-lista)))
    )
)
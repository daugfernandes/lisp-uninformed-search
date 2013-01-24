;;; =============================================================================
;   UNIVERSIDADE ABERTA                               Licenciatura em Informática
;   -------------------
;                                                            2º ano - 2º semestre
;   EFOLIO-A                           21071-Introdução à Inteligência Artificial
;
;   Aluno: David Augusto Paiva Fernandes (nº 902006)
;
;   xopuzzle.lsp
;
;;===============================================================================
;;                                  xopuzzle
;;===============================================================================

(defmacro xopuzzle-apply-move (board move)
  
  `(mapcar #'(lambda (x)
	       (xopuzzle-change-cell ,board (- (car x) 1) (- (cadr x) 1)))
	   (xopuzzle-cells-affected (length ,board) (car ,move) (cadr ,move))))

(defun xopuzzle-build (side initial-string)
  (assert (= (* side side) (length initial-string)))
  (loop for y from 1 to side
       collect 
       (loop for x from 1 to side
	  collect 
	    (if (char= #\x (char initial-string (- (+ x (* side (- y 1))) 1)))
		'x
		'o))))

(defun xopuzzle-equal (pseudo p1 p2)
  (setf pseudo t)
  (equal p1 p2))

(defmacro xopuzzle-change-cell (board row col)
  "Comuta o conteúdo de uma célula."
  `(setf (nth ,row (nth ,col ,board)) 
	 (if (equal (nth ,row (nth ,col ,board)) 
		    'x) 
	     'o 
	     'x)))

(defun xopuzzle-change-cell2 (cell)
  "Comuta o conteúdo de uma célula."
  (if (equal cell 'x) 'o 'x))

(defun xopuzzle-cells-count (board cells color)
  "Conta número de células de determinada côr"
   (count-if
    #'(lambda (x)
	(equal
	 (elt (elt board (- (cadr x) 1)) (- (car x) 1))
	 color))
    cells))
   
(defun xopuzzle-cells-affected (side row-or-col direction)
  "Calcula células afectadas por um determinado movimento."
  (let ((res nil))
    (mapcar 
     #'(lambda (x) 
	 (mapcar 
	  #'(lambda (y) 
	      (push-end res y))
	  x))
     (cond
       ((eq direction 'e)
	(loop for col from 1 to row-or-col 
	   collect 
	     (loop for row from 1 to side 
		collect (list col row))))
       ((eq direction 'd)
	(loop for col from row-or-col to side
	   collect 
	     (loop for row from 1 to side 
		collect (list col row))))
       ((eq direction 'c)
	(loop for col from 1 to side
	   collect 
	     (loop for row from 1 to row-or-col
		collect (list col row))))
       ((eq direction 'b)
	(loop for col from 1 to side
	   collect 
	     (loop for row from row-or-col to side
		collect (list col row))))))
    res))

(defun xopuzzle-suc-states (s)
  "Função `sucessores' de um estado."
  
  (let ((res nil)
	(aux nil)
	(cells-affected nil))
    
    (mapcar 
     #'(lambda (x) 
	 (mapcar 
	  #'(lambda (y)
	      (setf aux (copy-tree s))
	      (setf cells-affected (xopuzzle-cells-affected (length aux) (car y) (cadr y)))
	      (cond 
		((>= (xopuzzle-cells-count aux cells-affected 'x) 
		    (/ (length cells-affected) 2))
		 (xopuzzle-apply-move aux y)
		 (push-end res aux))))
	  x))
     (loop for rc from 1 to (length s)
	collect
	  (loop for direction in (list 'e 'd 'c 'b)
	     collect (list rc direction))))
    res))

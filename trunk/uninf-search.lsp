;;; =============================================================================
;   UNIVERSIDADE ABERTA                               Licenciatura em Informática
;   -------------------
;                                                            2º ano - 2º semestre
;   EFOLIO-A                           21071-Introdução à Inteligência Artificial
;
;   Aluno: David Augusto Paiva Fernandes (nº 902006)
;
;   uninf-search.lsp
;
;;===============================================================================
;;                               Procuras cegas
;;===============================================================================

; estrutura de uma geração; para mais detalhes ver `initialize-generation'.
(defstruct generation order state level parent expansion)

(defun initialize-generation (order state level parent expansion)
  "Helper para construção de uma `geração'."
  (make-generation 
   :order order              ; ordinal de geração
   :state state              ; estado
   :level level              ; nível de expansão
   :parent parent            ; pai
   :expansion expansion))    ; ordinal de expansão

(defun ancestors (gen order)
  "Função recursiva de obtenção da cadeia de parents."
  (cond 
    ((equal order 1) (list 1))
    (t
     (append (list order) (ancestors gen (generation-parent (nth (- order 1) gen)))))))
    
(defun solution-path (gen)
  "Extrai o caminho da solução começando no final e invertendo o resultado."
  (let ((res nil)
	(generation-solution (find-if  
			      #'(lambda (x) (equal (generation-expansion x) '*))
			      (cadr gen))))
    (if (null generation-solution)
	(setf res nil)
	(setf res (reverse (ancestors (cadr gen) (generation-order generation-solution)))))
    res))

(defun counters (found-solution gen)
  "Extrai informação de contagem de uma solução."
   (print 
    (format nil "Solution=~a~15,5Tgen=~a~25,5Texp=~a~35,5Tnotexp=~a~50,5Tlevel=~a~%=> path: ~a "
	    found-solution
	    (length (cadr gen))
	    (count-if #'(lambda (x) (generation-expansion x)) (cadr gen))
	    (count-if #'(lambda (x) (null (generation-expansion x))) (cadr gen))
	    (let ((generation-solution (find-if  
					#'(lambda (x) (equal (generation-expansion x) '*))
					(cadr gen))))
	      (if (null generation-solution)
		  nil
		  (generation-level generation-solution)))
	    (solution-path gen))))

(defun analyse-generations (gen)
  "Output pretty-print da (não)solução."
  (cond 
    ((null (car gen))
     (counters "No" gen))
    ((listp (car gen)) ; incremental
     (mapcar #'(lambda (g) (analyse-generations g)) gen))
    (t
     (counters "Yes" gen))))

; estrutura de um problema; para mais detalhes ver `initialize-problem'.
(defstruct problem name initial-state final-states state-equal-fn successors-fn maximum-level)

(defun initialize-problem (name initial-state final-states state-equal-fn
			   successors-fn maximum-level)
  "Helper para construção de um `problema'."
  (make-problem 
   :name name                      ; nome do problema
   :initial-state initial-state    ; estado inicial
   :final-states final-states      ; lista de estados finais
   :state-equal-fn state-equal-fn  ; função-igualdade de estados
   :successors-fn successors-fn    ; função-sucessores de um estado
   :maximum-level maximum-level))  ; nível máximo de expansão



; utilidades
(defun find-in (element list equal-fn)
  "Procura um elemento `element' na lista `list'
   utilizando uma função-igualdade (1) especificada
   externamente."
  (find-if 
   #'(lambda (x) 
       (funcall equal-fn nil element x)) ; (1) o primeiro argumento da função-igualdade,
   list))                                ; se T, indica a utilização de uma pseudo-igualdade
                                         ; que a função deve implementar.
                                         ; útil, por exemplo, para eliminar simetrias
                                         ; ou outras redundâncias. (ver `find-in-expansions').

(defun find-in-expansions (state expansions equal-fn)
  "Procura um `expansions' pela ocorrência de um `state'.
   (ver comentários a `find-in')."
  (find-if 
   #'(lambda (x) 
       (funcall equal-fn t state (generation-state x)))
   expansions))

(defmacro push-end (list item)
  "Adiciona `item'no final de `list'."
  `(if (null ,list)
       (setf ,list (list ,item))
     (nconc ,list (list ,item))))

(defmacro pop-end (place)
  "Retira tail da lista `place'."
  `(prog1
       (first (last ,place))
     (if (null (second ,place))
	 (setf ,place nil)
       (rplacd (last ,place 2) nil))))


; algoritmos de estratégias

(defun breath-first-search (expansions)
  "Função de selecção de próximo estado a expandir utilizando
   o algoritmo largura-primeiro FIFO."
  (when (not (null expansions))
    (loop for x in expansions
	  when (null (generation-expansion x))
	  do (return x))))

(defun depth-first-search (expansions)
  "Função de selecção de próximo estado a expandir utilizando
   o algoritmo de profundidade-primeiro."
  (let ((ret nil)
	(ret-level (- 1)))
    (loop for x in expansions
       when (null (generation-expansion x))
       do (cond ((> (generation-level x) ret-level)
		 (setf ret x)
		 (setf ret-level (generation-level x)))))
    ret))

; algorimo central de procura

(defun tree-search (problem strategy)
  "Função geral que implementa a procura de soluções de um problema
   mediante uma função que implementa uma determinada estratégia:
   largura-primeiro ou profundidade-primeiro."


  (let* ((order 1)
	 (level 0)
	 (parent 0)
	 (expansion 0)
	 (act-generation nil)
	 (found nil)
	 (aux nil)
	 (maximum-level (problem-maximum-level problem))
	 (expansions (list 
		      (initialize-generation 
		       order 
		       (problem-initial-state problem)
		       level 
		       parent 
		       nil))))
	   
    ; se o estado inicial é um dos finais ... nada a fazer; encontrou.
    (cond
	((find-in (problem-initial-state problem)
		  (problem-final-states problem)
		  (problem-state-equal-fn problem))
	 (setf (generation-expansion (elt expansions 0)) '*)
	 (setf found t))
	(t ; caso contrário prepara o início do ciclo
	 (incf level)
	 (setf act-generation (funcall strategy expansions))))

    ; ciclo
    (loop while (and (null found) 
		     (not (null act-generation)))
       do
	 (setf (generation-expansion act-generation) 
	       (incf expansion))

	 (cond 
	   ((or (null maximum-level) 
		(< (generation-level act-generation) maximum-level))
	    (setf aux 
		  (mapcar #'(lambda (x) 
			      (initialize-generation (incf order) 
						     x 
						     (+ 1 (generation-level act-generation)) 
						     (generation-order act-generation)  ; parent
						     nil))                              ; expansion
			  
			  (remove-if #'(lambda (x) (find-in-expansions  ; remove estados já gerados
						    x 
						    expansions 
						    (problem-state-equal-fn problem)))
				     (funcall (problem-successors-fn problem) ; gera estados sucessores
					      (generation-state act-generation)))))
	    
	    (when aux
	      (mapcar #'(lambda (x) (push-end expansions x)) aux)))) ; expande a lista de estados gerados.
	 
	 (setf act-generation (funcall strategy expansions))

	 (cond ((and (not (null act-generation))
		     (find-in (generation-state act-generation)
			      (problem-final-states problem)
			      (problem-state-equal-fn problem)))
		(setf (generation-expansion act-generation) '*)
		(setf found t))))

    (list found expansions)))

(defun incremental-tree-search (problem strategy maximum-level)
  "Função geral que implementa o algoritmo de procura incremental."
  (let ((aux nil)
	(ret nil))
    (loop for level from 1 to maximum-level do
	 (setf (problem-maximum-level problem) level)
	 (setf aux (tree-search problem strategy))
	 (push-end ret aux)
	 (when (car aux) ; encontrou solução
	   (return ret)))
    ret))


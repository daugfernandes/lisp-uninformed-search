;;; =============================================================================
;   UNIVERSIDADE ABERTA                               Licenciatura em Informática
;   -------------------
;                                                            2º ano - 2º semestre
;   EFOLIO-A                           21071-Introdução à Inteligência Artificial
;
;   Aluno: David Augusto Paiva Fernandes (nº 902006)
;
;   xopuzzle-test.lsp
;
;;===============================================================================
;;                                  xopuzzle
;;===============================================================================

(defun make-white-board (n)
  "Cria um tabuleiro branco com n casas."
  (if (not (zerop n))
      (concatenate 'string "o" (make-white-board (- n 1)))))


(defun ut-problem (side definition level)
  "Inicializa um problema e calcula a solução."
  (let ((prob 
	 (initialize-problem    (list side definition)
				(xopuzzle-build       side definition) 
				(list (xopuzzle-build side (make-white-board (* side side))))
				#'xopuzzle-equal 
				#'xopuzzle-suc-states 
				level)))

    (print (problem-name prob))
    (analyse-generations (tree-search prob #'depth-first-search))))


(defun all (level)
  "Função helper de cálculo de todos os problemas."
  (ut-problem 2 "xoox" level)
  (ut-problem 4 "xoxxxoxxoxoooxoo" level)
  (ut-problem 6 "xxoxxxxxoxxxxxoxxxxxoxxxooxoooxxoxxx" level)
  (ut-problem 5 "xoxoxoxoxooxoxooxoxooxoxo" level)
  (ut-problem 4 "oxoxxoxooxoxxoxo" level)
  (ut-problem 5 "xoxoxoxoxoxoxoxoxoxoxoxox" level)
  (ut-problem 6 "oxoxoxxoxoxooxoxoxxoxoxooxoxoxxoxoxo" level)
  (ut-problem 8 "xoxoxxoxoxoxooxoxoxoxxoxoxoxooxooxoxooxooxoxooxoxoxoxxoxoxoxooxo" level)
  (ut-problem 7 "xoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxox" level)
  (ut-problem 8 "oxoxoxoxxoxoxoxooxoxoxoxxoxoxoxooxoxoxoxxoxoxoxooxoxoxoxxoxoxoxo" level)
  (ut-problem 10 "oxxoxxxooxxooxoooxxoxooxoooxxooxxoxxxooxoxxoxxxooxoxxoxxxooxxooxoooxxoxooxoooxxooxxoxxxooxoxxoxxxoox" level)
  (ut-problem 10 "oxoxoxoxoxxoxoxoxoxooxoxoxoxoxxoxoxoxoxooxoxoxoxoxxoxoxoxoxooxoxoxoxoxxoxoxoxoxooxoxoxoxoxxoxoxoxoxo" level)
  t)


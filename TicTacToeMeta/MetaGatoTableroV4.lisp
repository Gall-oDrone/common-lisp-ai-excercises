
;;;======================================================================================
;;;  TicTacToe.lisp
;;;      Resuelve el problema de de Gato 4x4.
;;;   
;;;      Representación de los estados: 
;;;         Un valor númérico mayor a 1 y menor a 16 que representa.
;;;			la posición tomada por un jugador en el tablero. 
;;;         El tablero esta formado por 4 files y 4 columnas
;;;                 Estado inicial:        Estado meta:
;;;                 Casilla elegida		  Ganar la partida de GATO
;;;					 por jugador X        1 filas, columna o diagonale con X's ó O's
;;;
;;;  Diego Gallo Valenzuela.
;;;  Diciembre, 2020
;;;======================================================================================
(defparameter  *open* '())    
(defparameter  *memory* '())  

(defparameter  *tablero3* '())
(defparameter  *parent_id* 1)
(defparameter  *tablero* '()) 
(defparameter  *tablero2* '())
(defparameter  *casilla_global* nil)
(defparameter  *casilla_local* nil)
(defparameter *turno* 1)
(defparameter  *id*  -1)
(defparameter  *casillas_ocupadas* '()) 
(defparameter  *current-ancestor*  nil)
(defparameter num 0)

(defun  create-board (id subcasillas estado)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado, al valor y al tipo de jugador como parámetro"
      (setf *tablero2* (append (list (list id estado subcasillas)) *tablero2*))
      (setf *tablero3* (append (list (list id estado subcasillas)) *tablero3*)))

(defun  create-node (estado valor jugador id_casilla_global)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado, al valor y al tipo de jugador como parámetro"
      (incf  *id*)
      (list  *id*  estado  *current-ancestor*  valor jugador id_casilla_global) ) 

(defun insert-to-open (estado  valor  metodo jugador id_casilla_global) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo"
     (let ((nodo  (create-node  estado  valor jugador id_casilla_global)))
         (cond ((eql  metodo  :depth-first)
	                  (push  nodo  *open*))
	   	   (T  Nil)))  )

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))
      
(defun get-id-casilla-global (res)
  (let (
        (output1 nil)
        (output2 nil)
        (accum '())
        (rnum2 0)
        (cnum2 0)
        (rnum3 0)
        (cnum3 0)
        )
        ;; (print "(reverse *tablero3*)")
        ;; (print (reverse *tablero3*))
        (loop for e in (reverse *tablero2*) do
          (loop for r in e do
            (if (and (listp r) (> (length r) 2))
              (loop for c in r do
                (cond 
                  ((and (numberp res) (numberp c) (= res c)) 
                    (and 
                      ;; (setf (nth cnum2 (nth 2 (nth rnum2  (reverse *tablero2*)))) "X")
                      (setq output1 (nth 0 (nth rnum2  (reverse *tablero2*))))
                    ))
                  (T )
                )
                (incf cnum2)
              )
            )
            (setf cnum2 0)
          )
          (incf rnum2)
        )
        (setq output2 (get-sig-casillas res output1))
        (return-from get-id-casilla-global (list output1 output2 ))
  )
)

(defun get-sig-casillas (res id-casilla-global)
  (let (
        (output nil)
        (accum '())
        )
        (loop for e in (reverse *tablero3*) do
          (if (= (first e) id-casilla-global) (setq accum (append (reverse (third e)) accum)))
        )
        (loop for i from 0 to 8 do
          (if (= (nth i (reverse accum)) res) (setq output (+ i 1)))
        )
        (return-from get-sig-casillas output)
  )
)

(defun filtra-casillas-libres (tablero)
"Regresa una lista con todas las casillas libres del tablero" 
	(let ((nueva-lista nil))
		 (loop res for x in tablero do
	       (loop for y in x do
		   		(incf num)
		    	(if  (numberp y) (setq nueva-lista (push num nueva-lista)) nil))
		)
		(reverse nueva-lista)
	)
)
(defun filtra-casillas-ocupadas (tablero) 
"Regresa una lista con todas las casillas ocupadas por los dos jugadores en el tablero" 
	(let ((nueva-lista nil))
		 (loop res for x in tablero do
	       (loop for y in x do
		   		(incf num)
		    	(if  (not (numberp y)) (setq nueva-lista (push num nueva-lista)) nil))
		)
		(reverse nueva-lista)
	)
)

(defun available? (estado lista-casillas)
"Busca si la casilla o posición escogida por el jugador en turno esta disponible"  
     (cond ((null  lista-casillas)  T)
			( (equal  estado  (first  lista-casillas))  Nil)
		(T  (available?  estado  (rest  lista-casillas))))  )

(defun get-player-rows (estado tablero2 accum jugador casillas-hijas)
"Regresa el número total de filas con casillas tomadas por el jugador en turno"  
  (loop for r in casillas-hijas do
    (if (and (eq (first r) jugador) (eq (second r) jugador) (eq (third r) jugador)) (setq accum (+ accum 1)))
  )
  (return-from get-player-rows accum)
)
(defun get-player-columns (estado tablero2 accum jugador casillas-hijas)
"Regresa el número total de columnas con casillas tomadas por el jugador en turno"
  (if (and (eq (nth 0 (nth 0 casillas-hijas) jugador)) (eq (nth 0 (nth 1 casillas-hijas) jugador)) (eq (nth 0 (nth 2 casillas-hijas) jugador))) (setq accum (+ accum 1)))
  (if (and (eq (nth 1 (nth 0 casillas-hijas) jugador)) (eq (nth 1 (nth 1 casillas-hijas) jugador)) (eq (nth 1 (nth 2 casillas-hijas) jugador))) (setq accum (+ accum 1)))
  (if (and (eq (nth 0 (nth 2 casillas-hijas) jugador)) (eq (nth 1 (nth 2 casillas-hijas) jugador)) (eq (nth 2 (nth 2 casillas-hijas) jugador))) (setq accum (+ accum 1)))
  (return-from get-player-columns accum)
)
(defun get-player-diagonals (estado tablero2 accum jugador)
"Regresa el número total de diagonales con casillas tomadas por el jugador en turno"
	(if (and (eq (nth 0 (nth 0 casillas-hijas) jugador)) (eq (nth 1 (nth 1 casillas-hijas) jugador)) (eq (nth 2 (nth 2 casillas-hijas) jugador))) (setq accum (+ accum 1)))
  (if (and (eq (nth 2 (nth 0 casillas-hijas) jugador)) (eq (nth 1 (nth 1 casillas-hijas) jugador)) (eq (nth 0 (nth 2 casillas-hijas) jugador))) (setq accum (+ accum 1)))
  (return-from get-player-diagonals accum)
)

(defun get-rows (estado casillas-libres accum)
"Regresa el número total de filas con casillas libres dada una posición o casilla"  
	(loop for r in casillas-hijas do
    (if (and (numberp (first r)) (numberp (second r)) (numberp (third r))) (setq accum (+ accum 1)))
  )
  (return-from get-player-rows accum)
)
(defun get-columns (estado casillas-libres accum)
"Regresa el número total de columnas con casillas libres dada una posición o casilla"  
	(if (and (numberp (nth 0 (nth 0 casillas-hijas))) (numberp (nth 0 (nth 1 casillas-hijas))) (numberp (nth 0 (nth 2 casillas-hijas)))) (setq accum (+ accum 1)))
  (if (and (numberp (nth 1 (nth 0 casillas-hijas))) (numberp (nth 1 (nth 1 casillas-hijas))) (numberp (nth 1 (nth 2 casillas-hijas)))) (setq accum (+ accum 1)))
  (if (and (numberp (nth 0 (nth 2 casillas-hijas))) (numberp (nth 1 (nth 2 casillas-hijas))) (numberp (nth 2 (nth 2 casillas-hijas)))) (setq accum (+ accum 1)))
  (return-from get-player-columns accum)
)
(defun get-diagonals (estado casillas-libres accum)
"Regresa el número total de filas con diagonales libres dada una posición o casilla"  
	(if (and (numberp (nth 0 (nth 0 casillas-hijas))) (numberp (nth 1 (nth 1 casillas-hijas))) (numberp (nth 2 (nth 2 casillas-hijas)))) (setq accum (+ accum 1)))
  (if (and (numberp (nth 2 (nth 0 casillas-hijas))) (numberp (nth 1 (nth 1 casillas-hijas))) (numberp (nth 0 (nth 2 casillas-hijas)))) (setq accum (+ accum 1)))
)

(defun get-casillas-global-a-local (estado id-casilla-global)
  (let (
        (accum nil)
      )
    (loop for e in *tablero3* do
      (if (= (first e) id-casilla-global) (setq accum (append (list (reverse (third e))) accum)))
    )
    (return-from get-casillas-global-a-local (reverse accum))
  )
)

(defun put-tablero2 (estado id-casilla-global jugador)
  (let (
        (r0 0)
        (c0 0)
      )
    (loop for e in *tablero2* do
      (if (= (first e) id-casilla-global) 
        (loop for c in (third e) do 
          (if (and (numberp c) (= c estado)) 
              (and (setf (nth c0 (reverse (third (nth r0 (reverse *tablero2*))))) jugador))
              (return-from put-tablero2 T))
          (setq c0 (+ c0 1))
        )
      )
      (setq c0 0)
      (setq r0 (+ r0 1))
    )
    (return-from put-tablero2 nil)
  )
)

(defun filtra-ops (estado casilla_local id-casilla-global)
  (let (
        (accum nil)
        (accum2 nil)
        (ops_validos nil)
        (id-casilla-res nil)
      )
    (loop for e in *tablero3* do
      (if (= (first e) casilla_local) (setq accum (append (third e) accum)))
    )
    (loop for i in accum do
      (if (numberp i) (setq ops_validos (cons i ops_validos)))
    )
    ;; (loop for i from 0 to 8 do
    ;;   (if (and (numberp (nth i accum)) (= estado (nth  i accum))) (setq id-casilla-res i))
    ;; )
    ;; (loop for e in *tablero2* do
    ;;   (if (= (first e) id-casilla-res) (setq accum2 (append (third e) accum2)))
    ;; )
    ;; (loop for i in accum2 do
    ;;   (if (numberp i) (setq ops_validos (cons i ops_validos)))
    ;; )
    (return-from filtra-ops (reverse ops_validos))
  )
)

(defun Turno (jugador)
"Revisa si el jugador maximizar su estrategia corresponde a todos los turnos impares"
	(if (and (oddp *turno*) (= jugador 'X)) T nil)
)
(defun SetJugador  (turno)
"Regresa el tipo de jugador dependiendo de si el turno en cuestión es par o impar"
	(if (evenp turno) 'O 'X)
)

(defun  evalua-estado (estado)
"Predicado. Valida  un estado según las restricciones generales del problema...
       el estado tiene estructura:  [(<r0><c0>)(<r1><c1>)]"
	   (let  (
	    (sucesores nil)
		)
		(setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
				   (loop for  element  in  sucesores  do
					(insert-to-open  (first element)  (second element)  :depth-first  jugador))
	))


(defun  valid-state? (estado jugador id-casilla-global)
"Predicado. Valida  un estado según las restricciones generales del problema...
       el estado tiene estructura:  [(<casilla><r0><c0>)]"
	   (let*  (
       (casilla-elegida (first estado))
       (nueva_id_casilla_global (first (get-id-casilla-global casilla-elegida)))
       (nueva_id_casilla_local (second (get-id-casilla-global casilla-elegida)))
		   (r0 (second estado))
		   (c0 (third estado))
		   (nodo nil)
		   (resp nil)
		   )
       (if (and 
                (put-tablero2 casilla-elegida nueva_id_casilla_global jugador) 
                (setf (nth c0 (nth r0 *tablero*)) jugador))
                (insert-to-open casilla-elegida nil  :depth-first  jugador nueva_id_casilla_global)
       )
			 (setq   nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
					estado  (second  nodo) 
					jugador (fifth nodo)
          nueva_id_casilla_global (sixth nodo)) 
				   (push  nodo  *memory*)  			;;Recordarlo antes de que algo pueda pasar...
	))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  (
	       (operador op))     ;; este operador es la etiqueta numérica del operador...
	(case operador
	 	(1 (list 1 0 0))
		(2 (list 2 0 1))
		(3 (list 3 0 2))
		(4 (list 4 1 0))
		(5 (list 5 1 1))
		(6 (list 6 1 2))
		(7 (list 7 2 0))
		(8 (list 8 2 1))
		(9 (list 9 2 2))
		(10 (list 10 0 0))
		(11 (list 11 0 1))
		(12 (list 12 0 2))
		(13 (list 13 1 0))
		(14 (list 14 1 1))
		(15 (list 15 1 2))
		(16 (list 16 2 0))
    (17 (list 17 2 1))
    (18 (list 18 2 2))
    (19 (list 19 0 0))
    (20 (list 20 0 1))
    (21 (list 21 0 2))
		(22 (list 22 1 0))
		(23 (list 23 1 1))
		(24 (list 24 1 2))
		(25 (list 25 2 0))
		(26 (list 26 2 1))
    (27 (list 27 2 2))
    (28 (list 28 3 0))
    (29 (list 29 3 1))
    (30 (list 30 3 2))
    (31 (list 31 4 0))
		(32 (list 32 4 1))
		(33 (list 33 4 2))
		(34 (list 34 5 0))
		(35 (list 35 5 1))
		(36 (list 36 5 2))
    (37 (list 37 3 0))
    (38 (list 38 3 1))
    (39 (list 39 3 2))
    (40 (list 40 4 0))
    (41 (list 41 4 1))
		(42 (list 42 4 2))
		(43 (list 43 5 0))
		(44 (list 44 5 1))
		(45 (list 45 5 2))
		(46 (list 46 3 0))
    (47 (list 47 3 1))
    (48 (list 48 3 2))
    (49 (list 49 4 0))
    (50 (list 50 4 1))
    (51 (list 51 4 2))
		(52 (list 52 5 0))
		(53 (list 53 5 1))
		(54 (list 54 5 2))
		(55 (list 55 6 0))
		(56 (list 56 6 1))
    (57 (list 57 6 2))
    (58 (list 58 7 0))
    (59 (list 59 7 1))
    (60 (list 60 7 2))
    (61 (list 61 8 0))
		(62 (list 62 8 1))
		(63 (list 63 8 2))
		(64 (list 64 6 0))
		(65 (list 65 6 2))
		(66 (list 66 6 3))
    (67 (list 67 7 3))
    (68 (list 68 7 3))
    (69 (list 69 7 3))
    (70 (list 70 8 3))
    (71 (list 71 8 1))
		(72 (list 72 8 2))
		(73 (list 73 6 0))
		(74 (list 74 6 1))
		(75 (list 75 6 2))
		(76 (list 76 7 0))
    (77 (list 77 7 1))
    (78 (list 78 7 2))
    (79 (list 79 8 0))
    (80 (list 80 8 1))
    (81 (list 81 8 2))
	  (T "error"))))
;; (defun FinDeJuego ( &optional terminated)
;; "Revisa el número total de casillas ya han sido ocupadas o si algún jugador ganó la partida"
;; (let* (
;; 		(accum-row 0)
;; 		(accum-column 0)
;; 		(accum-diag 0)
;; 		(accum-row-other 0)
;; 		(accum-column-other 0)
;; 		(accum-diag-other 0)
;; 		(other-player (if (eql jugador 'O) 'X 'O))
;; 		(player-rows (get-player-rows estado *tablero2* accum-row jugador))
;; 		(player-columns (get-player-columns estado *tablero2* accum-column jugador))
;; 		(player-diagonals (get-player-diagonals estado *tablero2* accum-diag jugador))
;; 		(other-player-rows (get-player-rows estado *tablero2* accum-row-other other-player))
;; 		(other-player-columns (get-player-columns estado *tablero2* accum-column-other other-player))
;; 		(other-player-diagonals (get-player-diagonals estado *tablero2* accum-diag-other other-player))
;; 	) 
;; 	(if (= (length (filtra-casillas-ocupadas *tablero*)) 15) T 	
;; 		(if (or (= player-rows 4) (= player-columns 4) (= player-diagonals 4) 
;; 	       (= other-player-rows 4) (= other-player-columns 4) (= other-player-diagonals 4))) T nil)
;; 	)
;; )

(defun Valor_NegaMax (estado id_casilla_local id_casilla_global jugador profundidad maxProf)
	(let ((MejorValor nil)
		  (MejorMov nil)
		  (valor nil)
		  (nuevo-estado nil)
		  (ops (filtra-ops estado id_casilla_local id_casilla_global))
		  (lista nil))
	;; (if (= profundidad maxProf)
	;; 	(evalua-estado estado)
	;; 	(progn (setq MejorMov nil) (if (Turno jugador) (setq MejorValor most-negative-fixnum) (setq MejorValor most-positive-fixnum))))
	(dolist (op ops lista)
		(setq nuevo-estado (apply-operator op estado))
		(if (valid-state? nuevo-estado jugador id_casilla_global) (setf *turno* (+ *turno* 1)) (return-from Valor_MiniMax nil))
		;; (setq valor (Valor_NegaMax nuevo-estado (SetJugador *turno*) (+ profundidad 1) maxProf))
	)
  ;;   (if (Turno jugador) 
	;; 		(when (> valor MejorValor) 
	;; 			(setq MejorValor valor)
	;; 			(setq MejorMov op))
	;; 		(when (< valor MejorValor) 
	;; 			(setq MejorValor valor)
	;; 			(setq MejorMov op)))
	;; (list MejorMov MejorValor )
	)
)

(defun model (input)
"Realiza una búsqueda a lo profundo, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :miniMax (default)
                               :poda alpha-beta"
  (let ((nodo nil)
	  (estado nil)
	  (sucesores '())
	  (profundidad  4)
	  (jugador nil)
	  (casilla_elegida input)
    (id_casilla_global (first (get-id-casilla-global input)))
    (id_casilla_local (second (get-id-casilla-global input)))
	  (metodo  :depth-first))
	  (insert-to-open casilla_elegida nil metodo "X" id_casilla_global)
	  (setq   nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
				estado  (second  nodo) 
				valor   (fourth nodo)           ;;Identificar el estado y operador que contiene
				jugador (fifth nodo))             
		
		(setq  *current-ancestor*  (first  nodo))
		(first (Valor_NegaMax input id_casilla_local id_casilla_global "O" 3 4))))


(defun UserInput()
  (terpri)
  (princ "Ingrese un número de casilla valido: ")
  (setq respuesta (read))
  (princ "Jugador X eligió: ")
  (write respuesta)
)

(defun set-board ( &optional input)
	(let ((board nil)
		  (accum nil)
      (accum2 nil)
      (p_id 0)
      (row 0)
      (column 0)
      (r1 nil)
      (rnum 0)
      (cnum 0)
      (rnum2 0)
      (cnum2 0)
      (counter 0)
      (res input)
		)
    (if (= *turno* 1) 
      (loop for i from 1 to 81 do
        (setq accum (cons i accum))
        (if (= (mod i 3) 0) 
          (progn 
            (setf *tablero* (push (reverse accum) *tablero*)) 
            (cond 
              ((and (= (mod i 9) 0) (< i 27) (create-board (setq p_id (+ p_id 1)) (reverse accum) nil)) (setq p_id 0))
              ((and (= (mod i 9) 0) (>= i 27) (< i 54) (create-board (setq p_id (+ p_id 1)) (reverse accum) nil)) (setq p_id 3))
              ((and (= (mod i 9) 0) (>= i 54) (create-board (setq p_id (+ p_id 1)) (reverse accum) nil)) (setq p_id 6))
              (T nil)
            )
            (cond 
              ((and (= p_id 9) (>= i 54)) (setq p_id 6))
              ((and (= p_id 6) (>= i 27) (< i 54)) (setq p_id 3))
              ((and (= p_id 3) (< i 27)) (setq p_id 0))
              (T (if (= (mod i 9) 0) nil (create-board (setq p_id (+ p_id 1)) (reverse accum) nil)))
            )
            (setq accum '())
          )
        )
		  )
      (progn
        (loop for r in (reverse *tablero*) do
          (loop for c in r do
            (cond 
              ((and (numberp res) (numberp c) (= res c)) 
                (setf (nth cnum (nth rnum  (reverse *tablero*))) "X"))
              (T )
            )
            (incf cnum)
          )
          (incf rnum)
          (setf cnum 0)
        )
        (loop for e in (reverse *tablero2*) do
          (loop for r in e do
            (if (and (listp r) (> (length r) 2))
              (loop for c in r do
                (cond 
                  ((and (numberp res) (numberp c) (= res c)) 
                    (setf (nth cnum2 (nth 2 (nth rnum2  (reverse *tablero2*)))) "X"))
                  (T )
                )
                (incf cnum2)
              )
            )
            (setf cnum2 0)
          )
          (incf rnum2)
        )
      )
    )
    (setq board *tablero*)
    (loop for r in (reverse board) do
      (loop for c in r do
        (setq column (+ column 1))
        (if (< column 10) 
          (progn 
            (if (= (mod column 3) 0) 
              (if (< column 9) (format t "|  ~D | | " c) (format t "|  ~D |  " c))
              (format T "|  ~D " c))
            (if (= (mod column 9) 0) 
              (progn 
                (format t "~&") 
                (dotimes (i 11)
                  (format t " -- " c)
                )
                (format t "~&"))
            )) 
          (progn 
            (if (= (mod column 3) 0) (format t "| ~D | | " c) (format T "| ~D " c))
            (if (and (= (mod column 9) 0) (not (= row 8)) (not (= row 17)))
              (progn 
                (format t "~&") 
                (dotimes (i 11)
                  (format t "  -- " c)
                )
                (format t "~&"))
            ))
        ) 
        (if (or (and (= row 8) (= column 27)) (and (= row 17) (= column 54))) 
          (progn
            (format t "~&") 
            (dotimes (i 9)
            (if (and (< i 7) (= (mod (+ i 1) 3) 0)) (format t "  -    + " c) (format T "  -  "))
            (if (= i 8) (format t "~%")))
          )
            ) 
      )
      (setq row (+ row 1))
    )
    (setq res (UserInput))
    (format t "~&")
    (if 
      (and 
        (numberp res) 
        (>= res 1) 
        (<= res 81)
        (model res) 
      ) (set-board res) (return-from set-board nil))
	)
)
(set-board)

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
(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos
(defparameter  *tablero* '()) ;; Tablero del juego tictoe en donde cada casilla tiene asignada un número del 1 al 16
(defparameter  *tablero2* '((nil nil nil nil)(nil nil nil nil)(nil nil nil nil)(nil nil nil nil))) ;; Tablero del juego tictoe en donde cada casilla tiene asignado una O o una X
(defparameter  *casillas_ocupadas* '())  ;; Memoria de casillas ocupadas
(defparameter num 0)
(defparameter *turno* 2) 	  ;; Memoria de turnos
(defparameter  *ops*  '( (:Casilla-1        		1)
                         (:Casilla-2       			2)
                         (:Casilla-3  				3)
                         (:Casilla-4        		4)     
                         (:Casilla-5           		5)
						 (:Casilla-6           		6)
						 (:Casilla-7            	7)
						 (:Casilla-8            	8)
						 (:Casilla-9            	9)
						 (:Casilla-10            	10)
						 (:Casilla-11            	11)
						 (:Casilla-12            	12)
						 (:Casilla-13            	13)
						 (:Casilla-14            	14)
						 (:Casilla-15            	15)
						 (:Casilla-16            	16)))

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen

;;;=======================================================================================
;;  CREATE-NODE (estado  valor jugador)  
;;      estado - Un estado del problema a resolver (sistema)...
;;       valor - El valor generado de contar todos las files, columnas y diagonales
;;				disponibles que representen la victoria del jugador en turno...
;;;=======================================================================================
(defun  create-node (estado valor jugador)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado, al valor y al tipo de jugador como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  *id*  estado  *current-ancestor*  valor jugador) )  ;;los nodos generados son descendientes de *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :dfs     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun insert-to-open (estado  valor  metodo jugador) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo"
     (let ((nodo  (create-node  estado  valor jugador)))
         (cond ((eql  metodo  :depth-first)
	                  (push  nodo  *open*))
	        ;;    ((eql  metodo  :tictactoe)
		    ;;       (setq  *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  )


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))

(defun posicion-en-tablero (tablero)
"Regresa el número que corresponde a la casilla del tablero ocupada por el jugador X en el primer turno"
	(setq tablero *tablero*)
	(loop named res for x in tablero do
	       (loop for y in x do
		   		(incf num)
		    	(if  (and (not (null y)) (available? num *casillas_ocupadas*)) 
						(return-from res num)))))

(defun filtra-mayor-valor (valores)
	(setq valores (sort valores #'<))
	(first (reverse valores))
)

(defun filtra-menor-valor (valores)
	(setq valores (sort valores #'>))
	(first (reverse valores))
)
(defun Turno (jugador)
"Revisa si el jugador maximizar su estrategia corresponde a todos los turnos impares"
	(if (and (oddp *turno*) (= jugador 'X)) T nil)
)
(defun SetJugador  (turno)
"Regresa el tipo de jugador dependiendo de si el turno en cuestión es par o impar"
	(if (evenp turno) 'O 'X)
)
(defun filtra-casillas-libres (tablero)
"Regresa una lista con todas las casillas libres del tablero" 
	(let ((nueva-lista nil))
		 (loop res for x in tablero do
	       (loop for y in x do
		   		(incf num)
		    	(if  (eql y nil) (setq nueva-lista (push num nueva-lista)) nil))
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
		    	(if  (not (eql y nil)) (setq nueva-lista (push num nueva-lista)) nil))
		)
		(reverse nueva-lista)
	)
)
(defun filtra-ops (estado ops tablero)
"Regresa una lista con todas los operadores disponibles luego de que el jugador anterior haya tomado una posición" 
	(let ((nueva-lista nil))
		(dolist (op (second (first ops)) nueva-lista)
			(if (= estado op) nil (setq nueva-lista (push op nueva-lista)))
		)
		(reverse nueva-lista)
	)
)
(defun available? (estado lista-casillas)
"Busca si la casilla o posición escogida por el jugador en turno esta disponible"  
     (cond ((null  lista-casillas)  T)
			( (equal  estado  (first  lista-casillas))  Nil)
		(T  (available?  estado  (rest  lista-casillas))))  )

(defun get-player-rows (estado tablero2 accum jugador)
"Regresa el número total de filas con casillas tomadas por el jugador en turno"  
	(cond (
		((Null get-player-rows) accum)
		((and (or (>= 1 estado) (<= 4 estado)) (eql (car get-player-rows) jugador)) (get-rows estado (rest get-player-rows) (setq accum (+ accum 1)) jugador))
		((and (or (>= 5 estado) (<= 8 estado)) (eql (car get-player-rows) jugador)) (get-rows estado (rest get-player-rows) (setq accum (+ accum 1)) jugador))
		((and (or (>= 9 estado) (<= 12 estado)) (eql (car get-player-rows) jugador)) (get-rows estado (rest get-player-rows) (setq accum (+ accum 1)) jugador))
		((and (or (>= 13 estado) (<= 16 estado)) (eql (car get-player-rows) jugador)) (get-rows estado (rest get-player-rows) (setq accum (+ accum 1)) jugador))
		(T estado (rest get-player-rows) accum jugador)
	)
	)
)
(defun get-player-columns (estado get-player-columns accum jugador)
"Regresa el número total de columnas con casillas tomadas por el jugador en turno"
	(cond (
		((Null get-player-columns) accum)
		((and (or (= 1 estado) (= 5 estado) (= 9 estado) (= 13 estado)) (eql (car get-player-rows) jugador)) (get-columns estado (rest get-player-columns) (setq accum (+ accum 1)) jugador))
		((and (or (= 2 estado) (= 6 estado) (= 10 estado) (= 14 estado)) (eql (car get-player-rows) jugador)) (get-columns estado (rest get-player-columns) (setq accum (+ accum 1)) jugador))
		((and (or (= 3 estado) (= 7 estado) (= 11 estado) (= 15 estado)) (eql (car get-player-rows) jugador)) (get-columns estado (rest get-player-columns) (setq accum (+ accum 1)) jugador))
		((and (or(= 4 estado) (= 8 estado) (= 12 estado) (= 16 estado)) (eql (car get-player-rows) jugador)) (get-columns estado (rest get-player-columns) (setq accum (+ accum 1)) jugador))
		(T estado (rest get-player-columns) accum jugador)
	)
	)
)
(defun get-player-diagonals (estado get-player-diagonals accum jugador)
"Regresa el número total de diagonales con casillas tomadas por el jugador en turno"
	(cond (
		((Null get-player-diagonals) accum)
		((and (or (= 1 estado) (= 6 estado) (= 11 estado) (= 16 estado)) (eql (car get-player-rows) jugador)) (get-diagonals estado (rest get-player-diagonals) (setq accum (+ accum 1)) jugador))
		((and (or (= 4 estado) (= 7 estado) (= 10 estado) (= 13 estado)) (eql (car get-player-rows) jugador)) (get-diagonals estado (rest get-player-diagonals) (setq accum (+ accum 1)) jugador))
		(T estado (rest get-player-diagonals) accum jugador)
	)
	)
)

(defun get-rows (estado casillas-libres accum)
"Regresa el número total de filas con casillas libres dada una posición o casilla"  
	(cond (
		((Null casillas-libres) accum)
		((and (or (>= 1 estado) (<= 4 estado)) (or (>= 1 (car casillas-libres)) (<= 4 (car casillas-libres)))) (get-rows estado (rest casillas-libres) (setq accum (+ accum 1))))
		((and (or (>= 5 estado) (<= 8 estado)) (or (>= 5 (car casillas-libres)) (<= 8 (car casillas-libres)))) (get-rows estado (rest casillas-libres) (setq accum (+ accum 1))))
		((and (or (>= 9 estado) (<= 12 estado)) (or (>= 9 (car casillas-libres)) (<= 12 (car casillas-libres)))) (get-rows estado (rest casillas-libres) (setq accum (+ accum 1))))
		((and (or (>= 13 estado) (<= 16 estado)) (or (>= 13 (car casillas-libres)) (<= 16 (car casillas-libres)))) (get-rows estado (rest casillas-libres) (setq accum (+ accum 1))))
		(T estado (rest casillas-libres) accum)
	)
	)
)
(defun get-columns (estado casillas-libres accum)
"Regresa el número total de columnas con casillas libres dada una posición o casilla"  
	(cond (
		((Null casillas-libres) accum)
		((and (or (= 1 estado) (= 5 estado) (= 9 estado) (= 13 estado)) (or (= 1 (car casillas-libres)) (= 5 (car casillas-libres)) (= 9 (car casillas-libres)) (= 13 (car casillas-libres)))) (get-columns estado (rest casillas-libres) (setq accum (+ accum 1))))
		((and (or (= 2 estado) (= 6 estado) (= 10 estado) (= 14 estado)) (or (= 2 (car casillas-libres)) (= 6 (car casillas-libres)) (= 10 (car casillas-libres)) (= 14 (car casillas-libres)))) (get-columns estado (rest casillas-libres) (setq accum (+ accum 1))))
		((and (or (= 3 estado) (= 7 estado) (= 11 estado) (= 15 estado)) (or (= 3 (car casillas-libres)) (= 7 (car casillas-libres)) (= 11 (car casillas-libres)) (= 15 (car casillas-libres)))) (get-columns estado (rest casillas-libres) (setq accum (+ accum 1))))
		((and (or(= 4 estado) (= 8 estado) (= 12 estado) (= 16 estado)) (or (= 4 (car casillas-libres)) (= 8 (car casillas-libres)) (= 12 (car casillas-libres)) (= 16 (car casillas-libres)))) (get-columns estado (rest casillas-libres) (setq accum (+ accum 1))))
		(T estado (rest casillas-libres) accum)
	)
	)
)
(defun get-diagonals (estado casillas-libres accum)
"Regresa el número total de filas con diagonales libres dada una posición o casilla"  
	(cond (
		((Null casillas-libres) accum)
		((and (or (= 1 estado) (= 6 estado) (= 11 estado) (= 16 estado)) (or (= 1 (car casillas-libres)) (= 5 (car casillas-libres)) (= 9 (car casillas-libres)) (= 13 (car casillas-libres)))) (get-diagonals estado (rest casillas-libres) (setq accum (+ accum 1))))
		((and (or (= 4 estado) (= 7 estado) (= 10 estado) (= 13 estado)) (or (= 2 (car casillas-libres)) (= 6 (car casillas-libres)) (= 10 (car casillas-libres)) (= 14 (car casillas-libres)))) (get-diagonals estado (rest casillas-libres) (setq accum (+ accum 1))))
		(T estado (rest casillas-libres) accum)
	)
	)
)
(defun get-valor (estado casillas-libres jugador )
"Regresa el número total de filas, columnas y diagonales libres, asícomom las filas, columnas y diagonales ocupadas
por el jugador en turno. Si el número de casillas libres más el número de casillas ocupadas por el jugador en turno
es 4, entonces se suma 1 a las posiciones ganadores del jugador disponible o se suma 1 a las del otro jugador"
	(let* (
		(accum-row 0)
		(accum-column 0)
		(accum-diag 0)
		(other-player (if (eql jugador 'O) 'X 'O))
		(sum 0)
		(sum2 0)
		(rows (get-rows estado casillas-libres accum-row))
		(columns (get-columns estado casillas-libres accum-column))
		(diagonals (get-diagonals estado casillas-libres accum-diag))
		(player-rows (get-player-rows estado *tablero2* accum-row jugador))
		(player-columns (get-player-columns estado *tablero2* accum-column jugador))
		(player-diagonals (get-player-diagonals estado *tablero2* accum-diag jugador))
	) 
	(if (= (+ player-rows rows) 4) (setq sum (+ sum 1)) (setq sum2 (+ sum2 1)))
	(if (= (+ player-columns columns) 4) (setq sum (+ sum 1)) (setq sum2 (+ sum2 1)))
	(if (= (+ player-diagonals diagonals) 4) (setq sum (+ sum 1)) (setq sum2 (+ sum2 1)))
	(return-from get-valor (- sum sum2))
	))

(defun FinDeJuego ()
"Revisa el número total de casillas ya han sido ocupadas o si algún jugador ganó la partida"
(let* (
		(accum-row 0)
		(accum-column 0)
		(accum-diag 0)
		(accum-row-other 0)
		(accum-column-other 0)
		(accum-diag-other 0)
		(other-player (if (eql jugador 'O) 'X 'O))
		(player-rows (get-player-rows estado *tablero2* accum-row jugador))
		(player-columns (get-player-columns estado *tablero2* accum-column jugador))
		(player-diagonals (get-player-diagonals estado *tablero2* accum-diag jugador))
		(other-player-rows (get-player-rows estado *tablero2* accum-row-other other-player))
		(other-player-columns (get-player-columns estado *tablero2* accum-column-other other-player))
		(other-player-diagonals (get-player-diagonals estado *tablero2* accum-diag-other other-player))
	) 
	(if (= (length (filtra-casillas-ocupadas *tablero*)) 15) T 	
		(if (or (= player-rows 4) (= player-columns 4) (= player-diagonals 4) 
	       (= other-player-rows 4) (= other-player-columns 4) (= other-player-diagonals 4))) T nil)
	)
)

;; (defun NivelesDePrioridad
;; 	(case operador
;; 	 	(:ganar (list 1 0 0))
;; 		(:bloquear (list 2 0 1))
;; 		(:iniciar-centro  (list 3 0 2))
;; 	    (T "error"))
;; )
;;;=======================================================================================
;;  EXTRACT-LAST-OP Y REMEMBER-OP? (estado)
;;        extract-last-op: extra el último operador evaluado, anterior a la celda con esquinas
;;        remmeber-op? Revisa en *memory* el operador actual y lo filtra con la función anterior
;;;=======================================================================================
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado...
     el estado tiene estructura:  [(<r0><c0>)(<r1><c1>)],
     el operador tiene estructura : [<etiqueta-humana> <lista operador con (<bit>)>]"  
  (let*  (
	    (r0  (first (first estado)))
	    (c0  (second (first estado)))
		(r1  (first (second estado)))
		(c1  (second (second estado)))
		(operador (second (first op)))
		)
		(and  (> r0 0) 				;;la longitud mínima y máxima permitida para movimientos hacía arriba,
			  (<= r0  4)					;; hacia abajo, y a las esquinas
              (> c0 0)					;; se evita que salga de los bordes de la pared.
			  (<= c0  4)
			  (> r1 0)
			  (<= r1  4)
			  (> c1 0)
			  (<= c1  4))
	)  )  


;;;=======================================================================================
;;  VALID-STATE (estado)
;;        Predicado.  Indica si [estado] está disponible segun las restricciones del problema
;;                    Se llama a la función available? para comprobar si la casilla elegida está
;;					  disponible. También se llama a get-valor y se asigna la casilla elegida
;;					  por el jugador en turno en tablero tanto en el numérico como el que contiene
;;					  las casillas con el tipo de jugador
;;					
;;;=======================================================================================
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

(defun  valid-state? (estado jugador)
"Predicado. Valida  un estado según las restricciones generales del problema...
       el estado tiene estructura:  [(<casilla><r0><c0>)]"
	   (let*  (
	    (casillas-libres filtra-casillas-libres *tablero*)
		(casillas-ocupadas filtra-casillas-ocupadas *tablero*)
		(casilla-elegida (first estado))
		(r0 (second estado))
		(c0 (third estado))
		(nodo nil)
		(resp nil)
		)
		(if (available? casilla-elegida casillas-ocupadas) 
			(progn (setf (nth c0 (nth r0 *tablero*)) casilla-elegida) 
				   (setf (nth c0 (nth r0 *tablero2*)) jugador)
				   (insert-to-open casilla-elegida (get-valor casilla-elegida casillas-libres)  :depth-first  jugador)
				   (setq   nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
					estado  (second  nodo) 
					valor   (fourth nodo)           ;;Identificar el estado y operador que contiene
					jugador (fifth nodo)) 
				   (push  nodo  *memory*)  			;;Recordarlo antes de que algo pueda pasar...
					T
				  ) nil)
	))

;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================
(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  (
	       (operador op))     ;; este operador es la etiqueta numérica del operador...
	(case operador
	 	(1 (list 1 0 0))
		(2 (list 2 0 1))
		(3 (list 3 0 2))
		(4 (list 4 0 3))
		(5 (list 5 1 0))
		(6 (list 6 1 1))
		(7 (list 7 1 2))
		(8 (list 8 1 3))
		(9 (list 9 2 0))
		(10 (list 10 2 1))
		(11 (list 11 2 2))
		(12 (list 12 2 3))
		(13 (list 13 3 0))
		(14 (list 14 3 1))
		(15 (list 15 3 2))
		(16 (list 16 3 3))
	    (T "error"))))

;;;=======================================================================================
;;  VALOR_MINIMAX (estado jugador profundidad maxProf) 
;;	VALOR_PODA ALPHA-BETA (estado jugador profundidad maxProf, alpha, beta)
;;  Son las funciones principales que resuelven el juego del Gato 4x4
;;;=======================================================================================
(defun Valor_MiniMax (estado jugador profundidad maxProf)
	(let ((MejorValor nil)
		  (MejorMov nil)
		  (valor nil)
		  (nuevo-estado nil)
		  (ops (filtra-ops estado *Ops* *tablero*))
		  (lista nil))
	(if (or (FinDeJuego) (= profundidad maxProf))
		(evalua-estado estado)
		(progn (setq MejorMov nil) (if (Turno jugador) (setq MejorValor most-negative-fixnum) (setq MejorValor most-positive-fixnum))))
	(dolist (op ops lista)
		(setq nuevo-estado (apply-operator op estado))
		(if (valid-state? nuevo-estado jugador) (setf *turno* (+ *turno* 1)) (return-from Valor_MiniMax nil))
		(setq valor (Valor_MiniMax nuevo-estado (SetJugador *turno*) (+ profundidad 1) maxProf))
	)
    (if (Turno jugador) 
			(when (> valor MejorValor) 
				(setq MejorValor valor)
				(setq MejorMov op))
			(when (< valor MejorValor) 
				(setq MejorValor valor)
				(setq MejorMov op)))
	(list MejorMov MejorValor )
	)
)

(defun Valor_PodaAlpha-Beta (estado jugador profundidad maxProf, alpha, beta)
	(let ((MejorValor nil)
		  (MejorMov nil)
		  (valor nil)
		  (nuevo-estado nil)
		  (ops (filtra-ops estado *Ops* *tablero*))
		  (lista nil))
	(if (or (FinDeJuego) (= profundidad maxProf))
		(evalua-estado estado)
		(progn (setq MejorMov nil) (if (Turno jugador) (setq MejorValor most-negative-fixnum) (setq MejorValor most-positive-fixnum))))
	(dolist (op ops lista)
		(setq nuevo-estado (apply-operator op estado))
		(if (valid-state? nuevo-estado jugador) (setf *turno* (+ *turno* 1)) (return-from Valor_MiniMax nil))
		(setq valor (Valor_MiniMax nuevo-estado (SetJugador *turno*) (+ profundidad 1) maxProf, alpha, beta))
	)
    (if (Turno jugador) 
			(when (> valor MejorValor) 
				(setq MejorValor valor)
				(if (> valor alpha) (setq alpha valor) nil)
				(setq MejorMov op))
			(when (< valor MejorValor) 
				(setq MejorValor valor)
				(if (< valor beta) (setq beta valor) nil)
				(setq MejorMov op)))
	(list MejorMov MejorValor )
	)
)

;;;=======================================================================================
;;  MODEL (tablero) 
;;	funcion que llama al método Min Max o al de Poda Alpha Beta
;;;=======================================================================================
(defun model (tablero)
"Realiza una búsqueda a lo profundo, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :miniMax (default)
                               :poda alpha-beta"
  (let ((nodo nil)
	  (estado nil)
	  (sucesores '())
	  (profundidad  4)
	  (jugador nil)
	  (casilla (posicion-en-tablero tablero))
	  (metodo  :depth-first))
	  (setf *tablero* tablero)
	  (insert-to-open casilla nil metodo 'X)
	  (setq   nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
				estado  (second  nodo) 
				valor   (fourth nodo)           ;;Identificar el estado y operador que contiene
				jugador (fifth nodo))             
		
		(setq  *current-ancestor*  (first  nodo))
		(first (Valor_MiniMax casilla jugador 3 4))))

(defun tictactoe (tablero)
	(setq *output*
		(model tablero)
	))
;;;======================================================================================
;;;  Ranas-en-el-estanque.lisp
;;;      Resuelve el problema de las ranas en el estanque con búsqueda ciega, a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         Lista con una sublista interna, que representa las posiciones ocupadas por las ranas y el espacio disponible inicial. 
;;;         En cada orilla, número de ranas verdes(V), espacios en blanco disponibles(B) Y ranas cafés(C).
;;;                 Estado inicial:        Estado meta:
;;;				   ((1 1 1 0 2 2 2))	  ((2 2 2 0 1 1 1))
;;;
;;;  Gallo Valenzuela Diego
;;;  noviembre, 2020
;;;======================================================================================
(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos

(defparameter  *ops*  '( (:Una-Rana-Verde-salta-una-vez-hacia-la-derecha		 (1 0))
						 (:Una-Rana-Café-salta-una-vez-hacia-la-izquierda		(0 -1))
						 (:Una-Rana-Verde-salta-dos-veces-hacia-la-derecha    	 (2 0))
						 (:Una-Rana-Café-salta-dos-veces-hacia-la-izquierda		(0 -2))) )

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

;;;================================================================================02- Solución heurística.pdf=======
;;  CREATE-NODE (estado  op)  
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  *id*  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (cond ((eql  metodo  :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo  :breath-first)
		          (setq  *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  )


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))


;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los espacios vacios disponibles y la longitud de los brincos
;;;=======================================================================================
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado...
     el estado tiene estructura:  [(<v0><v1><v2><d><c1><c2><c3>)],
     el operador tiene estructura : [<etiqueta-humana> <lista operador con (<color rana><num posicion><num saltos>)>]"  
	T)


;;;=======================================================================================
;;  VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                          Es decir, si hay un espacio vació en la posición que la rana quiere saltar
;;;=======================================================================================
(defun  valid-state? (estado)
"Predicado. Valida  un estado según las restricciones generales del problema...
       el estado tiene estructura:  [(<o0><o1><o2><o3><o4><o5><o6>)]"
    (let (
		(p1  (nth 0 (first estado)))
		(p2  (nth 1 (first estado)))
		(p3  (nth 2 (first estado)))
	    (p4  (nth 3 (first estado)))
	    (p5  (nth 4 (first estado)))
		(p6  (nth 5 (first estado)))
		(p7  (nth 6 (first estado))))
      (or (= p4 2) ;;Verifica si la posición de las ranas es la indicada de acuerdo a los pasos previos para llegar al estado meta.
	  	  (and (= p2 1) (= p3 1))
	  	  (and (= p3 2) (= p5 1))
	  	  (and (= p3 2) (= p2 2) (p5 = 1) (p6 = 1))
	  	  (and (= p3 2) (= p2 2) (p1 = 2) (p5 = 1) (= p6 1) (= p7 1)))
	))

    
;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================

(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  (
	       (o0   		(nth 0 (first estado)))
		   (o1   		(nth 1 (first estado)))
		   (o2   		(nth 2 (first estado)))
		   (o3 	 		(nth 3 (first estado)))
		   (o4   		(nth 4 (first estado)))
		   (o5   		(nth 5 (first estado)))
		   (o6   		(nth 6 (first estado)))
	       (posición 	(first (second estado)))
	       (operador (first op)))     ;; este operador es la etiqueta humana del operador...
		   (print "operador en ao")
		   (print operador)
		   (print estado)
		   (and (eql posición nil) (list (list o0 o1 o2 o3 o4 o5 o6) (setq posición 0))) ;; verifica si posición es nil y, si sí es, asigna una posición de 0
	 (case operador 
	 	(:Una-Rana-Verde-salta-una-vez-hacia-la-derecha
		 	(cond
			 	((= posición 0) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				 (if (and (= 1 o0 ) (= 0 (nth (+ posición 1) (first estado))) )
				 	 (list (list (flip o0) (flip o1) o2 o3 o4 o5 o6) (list (+ posición 1)))
					 (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 1)))
				 )
				)
				((= posición 1) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				 (if (and (= 1 o1 ) (= 0 (nth (+ posición 1) (first estado))) )
				 	 (list (list o0 (flip o1) (flip o2) o3 o4 o5 o6) (list (+ posición 1)))
					  (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 1)))
				 )
				)
				;; ((= posición 2) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				;;  (if (and (= 1 o2 ) (= 0 (nth (+ posición 1) (first estado))) )
				;;  	 (list (list o0 o1 (flip o2) (flip o3) o4 o5 o6) (list (+ posición 1)))
				;; 	  (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 1)))
				;;  )
				;; )
				;; ((= posición 3) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				;;  (if (and (= 1 o3 ) (= 0 (nth (+ posición 1) (first estado))) )
				;;  	 (list (list o0 o1 o2 (flip o3) (flip o4) o5 o6) (list (+ posición 1)))
				;; 	  (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 1)))
				;;  )
				;; )
				;; ((= posición 4) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				;;  (if (and (= 1 o4 ) (= 0 (nth (+ posición 1) (first estado))) )
				;;  	 (list (list o0 o1 o2 o3 (flip o4) (flip o5) o6) (list (+ posición 1)))
				;; 	  (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 1)))
				;;  )
				;; )
				;; ((= posición 5) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				;;  (if (and (= 1 o5 ) (= 0 (nth (+ posición 1) (first estado))) )
				;;  	 (list (list o0 o1 o2 o3 o4 (flip o5) (flip o6)) (list (+ posición 1)))
				;; 	  (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 1)))
				;;  )
				;; )
				;; ((= posición 6) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				;;  (if (and (= 1 o5 ) (= 0 (nth (+ posición 1) (first estado))) )
				;;  	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 6)))
				;;  )
				;; )
		 	)
		)
	    (:Una-Rana-Verde-salta-dos-veces-hacia-la-derecha
		 	(cond
				((= posición 0) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				 (if (and (= 1 o0 ) (= 0 (nth (+ posición 2) (first estado)) ))
				 	 (list (flip o0) o1 (flip o2) o3 o4 o5 o6 (list (+ posición 1)))
					 (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 1)))
				 )
				)
				((= posición 1) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				 (if (and (= 1 o1 ) (= 0 (nth (+ posición 2) (first estado)) ))
				 	 (list (list o0 (flip o1) o2 (flip o3) o4 o5 o6) (list (+ posición 1)))
					 (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 1)))
				 )
				)
				;; ((= posición 2) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				;;  (if (and (= 1 o2 ) (= 0 (nth (+ posición 2) (first estado)) ))
				;;  	 (list (list o0 o1 (flip o2) o3 (flip o4) o5 o6) (list (+ posición 1)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 1)))
				;;  )
				;; )
				;; ((= posición 3) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				;;  (if (and (= 1 o3 ) (= 0 (nth (+ posición 2) (first estado)) ))
				;;  	 (list (list o0 o1 o2 (flip o3) o4 (flip o5) o6) (list (+ posición 1)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 1)))
				;;  )
				;; )
				;; ((= posición 4) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				;;  (if (and (= 1 o4 ) (= 0 (nth (+ posición 2) (first estado)) ))
				;;  	 (list (list o0 o1 o2 o3 (flip o4) o5 (flip o6)) (list (- posición 4)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 4)))
				;;  )
				;; )
				;; ((= posición 5) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				;;  (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 5)))
				;; )
				;; ((= posición 6) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				;;  (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 6)))
				;; )
		 	)
		)
		(:Una-Rana-Café-salta-una-vez-hacia-la-izquierda
		;; (print "culos")
		;; (print o5 ) 
		;; (print (first estado))
		;; (print posición)
		 	(cond
			 	((= posición 0) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				 (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 6)))
				)
				((= posición 1) ;; comprobar si hay un espacio vació en el lugar en donde la rana café quiere saltar
				 (if (and (= 2 o1 ) (= 0 (nth (- posición 1) (first estado))) )
				 	 (list (list (+ o0 2) (- o1 2) o2 o3 o4 o5 o6) (list (+ posición 5)))
					 (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 5)))
				 )
				)
				;; ((= posición 2) ;; comprobar si hay un espacio vació en el lugar en donde la rana café quiere saltar
				;;  (if (and (= 2 o2 ) (= 0 (nth (- posición 1) (first estado))) )
				;;  	 (list (list o0 (+ o1 2) (- o2 2) o3 o4 o5 o6) (list (- posición 1)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 1)))
				;;  )
				;; )
				;; ((= posición 3) ;; comprobar si hay un espacio vació en el lugar en donde la rana café quiere saltar
				;;  (if (and (= 2 o3 ) (= 0 (nth (- posición 1) (first estado))) )
				;;  	 (list (list o0 o1 (+ o2 2) (- o3 2) o4 o5 o6) (list (- posición 1)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 1)))
				;;  )
				;; )
				;; ((= posición 4) ;; comprobar si hay un espacio vació en el lugar en donde la rana café quiere saltar
				;;  (if (and (= 2 o4 ) (= 0 (nth (- posición 1) (first estado))) )
				;;  	 (list (list o0 o1 o2 (+ o3 2) (- o4 2) o5 o6) (list (- posición 1)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 1)))
				;;  )
				;; )
				;; ((= posición 5) ;; comprobar si hay un espacio vació en el lugar en donde la rana café quiere saltar
				;;  (if (and (= 2 o5 ) (= 0 (nth (- posición 1) (first estado))) )
				;;  	 (list (list o0 o1 o2 o3 (+ o4 2) (- o5 2) o6) (list (- posición 1)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 1)))
				;;  )
				;; )
				;; ((= posición 6) ;; comprobar si hay un espacio vació en el lugar en donde la rana café quiere saltar
				;;  (if (and (= 2 o6 ) (= 0 (nth (- posición 1) (first estado))) )
				;;  	  (list (list o0 o1 o2 o3 o4 (+ o5 2) (- o6 2)) (list (- posición 1)))
				;; 	  (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 1)))
				;;  )
				;; )
		 	)
		)
	    (:Una-Rana-Café-salta-dos-veces-hacia-la-izquierda
		 	(cond
			 	((= posición 0) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				 (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 6)))
				)
				((= posición 1) ;; comprobar si hay un espacio vació en el lugar en donde la rana quiere saltar
				 (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 5)))
				)
				;; ((= posición 2) ;; comprobar si hay un espacio vació en el lugar en donde la rana café quiere saltar
				;;  (if (and (= 2 o2 )  (= 0 (nth (- posición 2) (first estado)) ))
				;;  	 (list (list (+ o0 2) o1 (- o2 2) o3 o4 o5 o6) (list (+ posición 4)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (+ posición 4)))
				;;  )
				;; )
				;; ((= posición 3) ;; comprobar si hay un espacio vació en el lugar en donde la rana café quiere saltar
				;;  (if (and (= 2 o3 )  (= 0 (nth (- posición 2) (first estado)) ))
				;;  	 (list (list o0 (+ o1 2) o2 (- o3 2) o4 o5 o6) (list (- posición 1)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 1)))
				;;  )
				;; )
				;; ((= posición 4) ;; comprobar si hay un espacio vació en el lugar en donde la rana café quiere saltar
				;;  (if (and (= 2 o4 )  (= 0 (nth (- posición 2) (first estado)) ))
				;;  	 (list (list o0 o1 (+ o2 2) o3 (- o4 2) o5 o6) (list (- posición 1)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 1)))
				;;  )
				;; )
				;; ((= posición 5) ;; comprobar si hay un espacio vació en el lugar en donde la rana café quiere saltar
				;;  (if (and (= 2 o5 )  (= 0 (nth (- posición 2) (first estado)) ))
				;;  	 (list (list o0 o1 o2 (+ o3 2) o4 (- o5 2) o6) (list (- posición 1)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 1)))
				;;  )
				;; )
				;; ((= posición 6) ;; comprobar si hay un espacio vació en el lugar en donde la rana café quiere saltar
				;;  (if (and (= 2 o6 )  (= 0 (nth (- posición 2) (first estado)) ))
				;;  	 (list o0 o1 o2 o3 (+ o4 2) o5 (- o6 2) (list (- posición 1)))
				;; 	 (list (list o0 o1 o2 o3 o4 o5 o6) (list (- posición 1)))
				;;  )
				;; )
		 	)
		)
	    (T "error"))))


;;;=======================================================================================
;;  EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	     (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
		;;    (print "coño")
		;;    (print estado)
	         (setq  nuevo-estado  (apply-operator  op estado))  ;; primero se aplica el operador  y  después
			 (print "nuevo-estado")
			 (print nuevo-estado)
		;;  (when ;;(valid-state?  nuevo-estado)
	    ;;             (setq  descendientes  (cons  (list nuevo-estado op) descendientes)))
					(setq  descendientes  (cons  (list nuevo-estado op) descendientes)))) )


;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

;;;=======================================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;;=======================================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        
		 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
	     *solucion*))


(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Solución con ~A  pasos:~%~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~2A\)  aplicando ~20A se llega a ~A~%"  i (fourth  nodo)  (second  nodo)))))  )  ;; imprimir el número de paso, operador y estado...

;;;=======================================================================================
;;  RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       reset-all   Reinicializa todas las variables globales para una nueva ejecución
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;=======================================================================================
(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil))

(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))

      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...
	   (cond    ((equal  edo-meta  estado)  
		                (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (display-solution  (extract-solution  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
				 (print "nodo at bs method")
				 (print nodo)
			     (setq  sucesores  (expand estado))
				(print "succesores")
				 (print sucesores)
			     (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
			      (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)  metodo))))))  )
			     
     
;;;=======================================================================================
;;;=======================================================================================
;; (blind-search '((1 1 1 0 2 2 2)) '((2 2 2 0 1 1 1)) :breath-first)
(blind-search '((1 0 1 1 2 2 2)) '((2 2 2 0 1 1 1)) :breath-first)
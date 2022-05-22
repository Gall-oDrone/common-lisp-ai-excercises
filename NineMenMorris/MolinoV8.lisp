
;;;======================================================================================
;;;  NineMenMorrisV8.lisp
;;;      Resuelve el problema del Molino.
;;;   
;;;      Representación de los estados: 
;;;         Un valor númérico mayor a 1 y menor a 24 que representa
;;;			    la casilla tomada por un jugador en el tablero. 
;;;         El tablero esta formado por 8 files y 8 columnas
;;;         y cada una de ellas esta formada a su vez
;;;         por tres puntos o casillas vacías
;;;                 Estado inicial:        Estado meta:
;;;                 Casilla elegida		  Jugador O gana la partida 
;;;					          por jugador X      
;;;
;;;  Diego Gallo Valenzuela.
;;;  Enero, 2021
;;;======================================================================================
(defparameter  *open* '())    
(defparameter  *memory* '())  
(defparameter  *parent_id* 1)
(defparameter  *tablero* '()) 
(defparameter  *casillas_ocupadas* '())
(defparameter *turno* 1)
(defparameter  *id*  -1)
(defparameter  *id_fichas*  -1)
(defparameter  *fichas_disponibles_jugador_1*  '())
(defparameter  *fichas_disponibles_jugador_2*  '())
(defparameter  *fichas_ocupadas_jugador_1*  '())
(defparameter  *fichas_ocupadas_jugador_2*  '())
(defparameter  *current-ancestor*  nil)
(defparameter  *ops*  '(:row_win        	
                        :column_win         				
                        :row_block      	
                        :column_block         
                        :random_move      
                        ))
(defparameter  *heuristica*  '(:killer        	
                               :butterfly         				      	
                               :guard         
                              ))

;;;=======================================================================================
;;  CREATE-NODE (estado  valor jugador id_casilla_global)  
;;      estado - Un estado del problema a resolver (sistema)...
;;      valor - El valor generado de contar todos las files, columnas y diagonales
;;				      disponibles que representen la victoria del jugador en turno...
;;      id_casilla_global - Identificador de una casilla global de tamaño 3x3
;;;=======================================================================================
(defun  create-node (estado valor jugador id_casilla_global)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado, al valor, al tipo de jugador como parámetro
  y el id de las casillas globales (1,2,3,4,5,6,7,8,9)"
      (incf  *id*)
      (list  *id*  estado  *current-ancestor*  valor jugador id_casilla_global) ) 

;;;=======================================================================================
;;  CREATE-FICHAS ()  
;;   Genera las nueve fichas que cada jugador debe tener al inicio de la partida
;;   
;;;=======================================================================================
(defun  create-fichas ()
  "Construye dos listas con nueve casillas cada una, una para el jugador X y la otra para el jugador O"
      (dotimes (i 9)
        (incf  *id_fichas*)
        (setf *fichas_disponibles_jugador_1* (append (list (list *id_fichas* "X")) *fichas_disponibles_jugador_1*))
        (setf *fichas_disponibles_jugador_2* (append (list (list *id_fichas* "O")) *fichas_disponibles_jugador_2*))
      )
       
)
;;;=======================================================================================
;;  ASIGNAR-FICHA (jugador casilla)  
;;  jugador = jugador en turno
;;  casilla = casilla elegida por el jugador
;;  MOVER-FICHA (jugador casilla-origen casilla-destino)  
;;  jugador = jugador en turno
;;  casilla-origen = casilla ocupada por el jugador y la cual va a ser movida
;;  casilla-destino = casilla elegida por el jugador y en donde va a mover la casilla
;;  origen
;;  ELIMINAR-FICHA (jugador casilla)  
;;  jugador = jugador en turno
;;  casilla = casilla elegida por jugador y la cual va a ser eliminada del tablero
;;  ACTUALIZA-CASILLAS (jugador row-col-diag elemento &optional row)
;;  jugador = jugador en turno
;;  row-col-diag = lista que contiene la fila o columna que el jugador va a actualizar
;;  elemento = indica la posición (primero, segundo, tercero) de la casilla a actualizar
;;  row = indica la fila correspondiente en donde se ubica la casilla elegida
;;;=======================================================================================
(defun asignar-ficha (jugador casilla)
"Asigna la ficha o casilla elegida por el jugador en la lista de casillas ocupadas.
  Elimina una casilla menos de las casillas disponibles para mover en la Fase 1 del juego" 
  (let ((test nil))
    (if (string-equal jugador "X") 
      (progn
        (setq test (car *fichas_disponibles_jugador_1*))
        (setq test (append (list casilla) test))
        (setf *fichas_ocupadas_jugador_1* (append (list test) *fichas_ocupadas_jugador_1*)) 
        (setf *fichas_disponibles_jugador_1* (cdr *fichas_disponibles_jugador_1*)) 
        T
      )
      (progn
        (setq test (car *fichas_disponibles_jugador_2*))
        (setq test (append (list casilla) test))
        (setf *fichas_ocupadas_jugador_2* (append (list test) *fichas_ocupadas_jugador_2*)) 
        (setf *fichas_disponibles_jugador_2* (cdr *fichas_disponibles_jugador_2*)) 
      )
    )
  )
)
(defun mover-ficha (jugador casilla-origen casilla-destino) 
"Mueve la ficha o casilla origen elegida por el jugador en la lista de casillas ocupadas por
  la casilla destino. Luego, actualiza el tablero"
  (let (
        (row 0)
        (col 0)
       )
        (if (string-equal jugador "X") 
          (progn
            (loop for r in  *fichas_ocupadas_jugador_1* do
              (if (= (nth 0 r) casilla-origen)
                  (setf (nth 0 (nth row *fichas_ocupadas_jugador_1*)) casilla-destino)
              )
              (setq row (+ row 1))
            )
            (setf *fichas_ocupadas_jugador_1* (append (car *fichas_disponibles_jugador_1*))) 
            (setf *fichas_disponibles_jugador_1* (cdr *fichas_disponibles_jugador_1*)) 
          )
          (progn
            (loop for r in  *fichas_ocupadas_jugador_2* do
              (if (= (nth 0 r) casilla-origen)
                  (setf (nth 0 (nth row *fichas_ocupadas_jugador_2*)) casilla-destino)
              )
              (setq row (+ row 1))
            )
            (setf *fichas_ocupadas_jugador_2* (append (car *fichas_disponibles_jugador_2*))) 
            (setf *fichas_disponibles_jugador_2* (cdr *fichas_disponibles_jugador_2*)) 
          )
        )
        (setq row 0)
        (loop for i from 1 to 24 do
          (if (= casilla-origen i)
            (setf (nth col (nth row (reverse *tablero*))) casilla-origen)
          )
          (if (= col 2) (and (setq row (+ row 1)) (setq col 0)) (setq col (+ col 1)))
        )
  )
)
(defun eliminar-ficha (jugador casilla) 
"Elimina una de las fichas del otro jugador del tablero"
(print casilla)
  (let (
        (row 0)
        (col 0)
        (accum '())
       )
        (if (string-equal jugador "X") 
          (progn
            (loop for r in  *fichas_ocupadas_jugador_2* do
              (if (= (nth 0 r) casilla)
                nil
                (setq accum (append (list r) accum))
              )
            )
            (setf *fichas_ocupadas_jugador_2* accum)
          )
          (progn
            (loop for r in  *fichas_ocupadas_jugador_1* do
              (if (= (nth 0 r) casilla )
                nil
                (setq accum (append (list r) accum))
              )
            )
            (setf *fichas_ocupadas_jugador_1* accum)
          )
        )
        (loop for i from 1 to 24 do
          (if (= casilla i)
            (setf (nth col (nth row (reverse *tablero*))) casilla)
          )
          (if (= col 2) (and (setq row (+ row 1)) (setq col 0)) (setq col (+ col 1)))
        )
          (format t " Jugador ~D eliminó casilla: " jugador)
          (format t "~D " casilla)
          (format t "~&")

  )
)
(defun actualiza-casillas (jugador row-col-diag elemento &optional row)
"Actualiza la ficha elegida por el jugador y regresa una lista indicando el indice numérico de la casilla
 que se actualizó"
  (if (string-equal jugador "X")
            (progn
              (if (> (length *casillas_libres_jugador1*) 0)
                (progn 
                  (setf *casillas_libres_jugador1* (cdr *casillas_libres_jugador1*))
                  (setf (third (car *casillas_ocupadas_jugador1*)) 
                  (print row-col-diag)
                    (cond 
                      ((string-equal elemento "first") (progn (setf (first (nth (first row) *tablero*)) jugador) (first row-col-diag)))
                      ((string-equal elemento "second") (progn (setf (second (nth (second row) *tablero*)) jugador) (second row-col-diag)))
                      ((string-equal elemento "third") (progn (setf (third (nth (third row) *tablero*)) jugador) (third row-col-diag)))
                      (T nil)
                    )
                  )
                )
                (FinDeJuego)
              )              
            )
            (progn
              (if (> (length *fichas_disponibles_jugador_2*) 0)
                (progn 
                  (print elemento)
                  (print row)
                  (print "row-col-diag")
                  (print row-col-diag)
                  (print (first (nth (first row) (reverse *tablero*))))
                    (cond 
                      ((string-equal elemento "first")  (progn (asignar-ficha jugador (first (nth (first row) (reverse *tablero*))))  
                        (list T (first (first row-col-diag))) ) )
                      ((string-equal elemento "second") (progn (asignar-ficha jugador (second (nth (second row) (reverse *tablero*))))  
                        (list T (second (first row-col-diag))) ) )
                      ((string-equal elemento "third") (progn (asignar-ficha jugador (third (nth (third row) (reverse *tablero*)))) 
                        (list T (third (first row-col-diag))) ) )
                      (T nil)
                    )
                )
                (FinDeJuego)
              )              
            )
          ) 
)
;;;=======================================================================================
;;  GENERATE (jugador)  
;;  Función que crea una lista con todas las adyacencias posibles que el jugador artificial
;;  puede realizar.
;;;=======================================================================================
(defun generate (jugador )
"Revisa todas la posibles adyacencias que el jugador artificial puede realizar, dada una fila,
columna o diagonal. "
  (let (
    (row 0)
    (re 0)
    (ce 0)
    (which_col 0)
    (column 0)
    (accum 0)
    (accum2 0)
    (accum3 '())
    (accum4 '())
    (accum5 '())
    (accum6 '())
    (accum7 '())
    (accum8 '())
    (accum9 '())
    (accum10 '())
    (accum11 '())
    (columns '((1 10 22)
               (4 11 19)
               (7 12 16)
               (2  5  8)
               (9 13 18)
               (6 14 21)
               (3 15 24)
               (17 20 23)))
    (diags '((1 4 7)
             (3 6 9)
             (16 19 22)
             (18  21  24)))
  )
  (dotimes i 1 23
    (setq accum (append (list i)) accum)
    (if (= (mod row 3) 0) (and (setq accum2 (append (list (reverse accum)) accum2)) (setq row 0))
                          (setq row (+ row 1))
    )
  )
  ;; Generar Array de Adaycencias con puros números
  (loop for i from 0 to 23 do
    (loop for r in accum2 do
      (loop named eler for e in r do
        (if (eq i e) (progn (setq accum3 (append (list r) accum3)) (return-from elec nil)) )
      )
    )
    (loop for c in columns do
      (loop named elec for e in c do
        (if (eq i e) (progn (setq accum3 (append (list c) accum3)) (return-from elec nil)) )
      )
    )
    (loop for d in diags do
      (loop named eled for e in d do
        (if (eq i e) (progn (setq accum3 (append (list d) accum3)) (return-from eled nil)) )
      )
    )
    (setq accum4 (append (list (reverse accum3)) accum4)) 
  )
  ;; Loop forma de accum4 es [ [(1,2,3),(1,10,12),(1,9,14)] [(11,22,33),(1,10,12),(1,9,14)] ]
    (loop for el in accum4 do
      (loop for subel_1 in el do
        (loop for subel_2 in subel_1 do
          ;; Obtener accum4 pero ahora con posiciones libres y ocupadas y realizar análisis
          (dotimes i 0 23
            (if (= i subel_2)
              (set accum5 (append (list (nth ce (nth re *tablero*)) ) accum5))
              (set accum7 (append (list ce re ) accum7))
            )
            (if (= ce 2) 
              (progn 
                (setq ce 0)
                (setq re (+ re 1))
              ) 
              (setq ce (+ ce 1))
            )
          )
          (setq accum6 (append (list (reverse accum5) accum6)))
          (set accum8 (append (list (reverse accum7) ) accum8))
          (and (setq ce 0) (setq re 0) )
        )
        ;; accum 8 es un array [ (r1, c1), (r2,c2) (r3,c3) ]
        ;; accum 9 es un array [ [(r1, c1), (r2,c2) (r3,c3)] [(r4, c4), (r5,c5) (r6,c6)],... ]
        (set accum9 (append (list (reverse accum8) ) accum9))
      )
    )
    ;; accum 6 es un array tipo [ [(1,X,3),(1,O,O),(1,X,X)] [(11,X,O),(X,10,X),(1,9,14)] ]
    ;; accum 9 es un array tipo [ [ [ (r1, c1), (r2,c2) (r3,c3) ],
    ;;                              [ (r4, c4), (r5,c5) (r6,c6) ]
    ;;                              [ (r7, c7), (r8,c8) (r9,c9) ]
    ;;                            ]
    ;;                            [ ... ] 
    ;;                          ]
    (setq ce 0)
    (setq re 0)
    (loop for e in accum6 do
    ;;[(1,X,3),(1,O,O),(1,X,X)]
      (loop for sub_e_1 in e do
      ;;(1,X,3) 
        (cond 
          ;;(1, ?, ?)
          ((numberp (first sub_e_1)) 
            (cond 
              ;;Revisa si es un array potencial
              ;;(1, X, X)
              ( (and (eq (second sub_e_1) jugador) (eq (third sub_e_1) jugador) )
                (setq accum10 (append (list (first (nth ce (nth re accum9)))) accum10))
              )
              ;;Revisa si se cumple la adyacencia para el jugador en turno
              ;;(1, X, ?)
              ((eq (second sub_e_1) jugador)
               (progn 
                (setq accum11 (append (list (first (nth ce (nth re accum9)))) accum11))
                (setq which_col 1)
               )
              )
            )
          )
          ((numberp (second sub_e_1)) 
            (cond 
            ;;Revisa si es un array potencial
             ( (and (eq (first sub_e_1) jugador) (eq (third sub_e_1) jugador) )
               (setq accum10 (append (list (second (nth ce (nth re accum9)))) accum10))
             )
             ;;Revisa si hay se cumple la adyacencia para el jugador
              ((eq (first sub_e_1) jugador)
                (progn 
                  (setq accum11 (append (list (first (nth ce (nth re accum9)))) accum11))
                  (setq which_col 0)
                )
              )
              ((eq (third sub_e_1) jugador)
                (progn
                  (setq accum11 (append (list (third (nth ce (nth re accum9)))) accum11))
                  (setq which_col 2)
                )
              )
            )
          )
          ((numberp (third sub_e_1)) 
            (cond 
            ;;Revisa si es un array potencial
              ( (and (eq (first sub_e_1) jugador) (eq (second sub_e_1) jugador) )
                (setq accum10 (append (list (third (nth ce (nth re accum9)))) accum10))
              )
              ((eq (second sub_e_1) jugador)
                (progn 
                  (setq accum11 (append (list (second (nth ce (nth re accum9)))) accum11))
                  (setq which_col 1)
                )
              )
            )
          )
        )
        (if (and (> (length accum10) 0)
                 (> (length accum11) 0)
            )
            (progn
                ;; casilla destino coords en accum 10
                (setf (nth (second accum10) (nth (first accum10) *talero*)) jugador)
                ;; casilla origen coords en accum 10
                (setf (nth (second accum11) (nth (first accum11) *talero*)) 
                  (nth which_col (nth (second accum11) (nth (first accum11) accum4)))
                )
            )
            
        )
        (if (= ce 2) 
              (progn 
                (setq ce 0)
              ) 
              (setq ce (+ ce 1))
        )
      )
      (setq re (+ re 1))
      (setq accum10 nil)
      (setq accum11 nil)
    )
  )
)
;;;=======================================================================================
;;  CHECA-ADYACENCIA (jugador)  
;;  Revisa si hay espacios en blanco adyancetes a las casillas del jugador en turno.
;;;=======================================================================================
(defun checa-adyacencia (jugador)
  (let (
    (elemet 0)
  ))
  (loop for r in (reverse *tablero*) do
    (cond 
      ((and (numberp (first r)) ((eq (second r) jugador)) ))
      ((and ((eq (first r) jugador)) (numberp (second r)) ))
      ((and (numberp (second r)) ((eq (third r) jugador)) ))
    )
    (setq row (+ row 1))
  )
)

(defun checa-casilla-origen (input casillas-validas-jugador)
"Revisa si si la casilla elegida por el jugador humano es válida"
  (cond 
    ((Null (car casillas-validas-jugador)) nil)
    ((= input (car casillas-validas-jugador)) T)
    (T (checa-casilla-origen input (cdr casillas-validas-jugador)) )
  )
)
(defun checa-casilla-destino (input casillas-validas-jugador)
"Revisa si la casilla destino elegida por el jugador humano es válida"
  (cond 
    ((Null (car casillas-validas-jugador)) nil)
    ((eq input (car casillas-validas-jugador)) T)
    (T (checa-casilla-destino input (cdr casillas-validas-jugador)) )
  )
)
;;;=======================================================================================
;;  CASILLA-DESTINO (casilla jugador)  
;;  casilla = casilla en donde el jugador humano mueve la casilla origen
;;  jugador = jugador en turno
;;;=======================================================================================
(defun casilla-destino (casilla jugador)
"Genera la casilla destino y revisa si no es una casilla ocupada por algún jugador. Analiza si
hay una adyacencia en la casilla destino"
  (let (
    (accum '())
    (accum2 '())
    (row 0)
    (col 0)
    (col-o 0)
    (ud 0)
    (el nil)
    (cols '((1 10 22)(4 11 19)(7 12 16)(2 5 8)(9 13 18)(6 14 21)(3 15 24)(7 20 23)))
  )
    (loop for i from 1 to 24 do
      (if (= i casilla)
        (cond 
          ((and (= col 0) (check-var-type (nth (+ col 1) (nth row (reverse *tablero*))) "ganar" jugador "numero")) 
            (and (setq el "second") (setq accum (append (list (nth (+ col 1) (nth row (reverse *tablero*)))))))
          )
          ((and (= col 1) (check-var-type (nth (- col 1) (nth row (reverse *tablero*))) "ganar" jugador "numero")) 
            (and (setq el "first") (setq accum (append (list (nth (- col 1) (nth row (reverse *tablero*)))))))
          )
          ((and (= col 1) (check-var-type (nth (+ col 1) (nth row (reverse *tablero*))) "ganar" jugador "numero")) 
            (and (setq el "third") (setq accum (append (list (nth (+ col 1) (nth row (reverse *tablero*)))))))
          )
          ((and (= col 2) (check-var-type (nth (- col 1) (nth row (reverse *tablero*))) "ganar" jugador "numero")) 
            (and (setq el "second") (setq accum (append (list (nth (- col 1) (nth row (reverse *tablero*)))))))
          )
        )
      )
      (if (= col 2) (and (setq row (+ row 1)) (setq col 0)) (setq col (+ col 1)))
    )
    (if (null accum)
      (progn 
          (loop for e in cols do
            (loop for c in e do
              (if (= c casilla)
                (setq accum2 e)
              )
            )
          )
          (setq col 0)
          (setq row 0)
          (loop for ele in accum2 do
            (loop for i from 1 to 24 do
              (if (= i ele)
                (cond
                  ((= ele casilla) (setq col-o col)) 
                  ((and (= col-0 0) (= (+ col-o 1) col) (check-var-type (nth col (nth row (reverse *tablero*))) "ganar" jugador "numero")) 
                    (and (setq el "second") (setq accum (append (list (nth col (nth row (reverse *tablero*)))))))
                  )
                  ((and (= col-0 1) (= (+ col-o 1) col) (check-var-type (nth col (nth row (reverse *tablero*))) "ganar" jugador "numero")) 
                    (and (setq el "third") (setq accum (append (list (nth col (nth row (reverse *tablero*)))))))
                  )
                  ((and (= col-0 1) (= (- col-o 1) col) (check-var-type (nth col (nth row (reverse *tablero*))) "ganar" jugador "numero")) 
                    (and (setq el "first") (setq accum (append (list (nth col (nth row (reverse *tablero*)))))))
                  )
                  ((and (= col-0 2) (= (- col-o 1) col) (check-var-type (nth col (nth row (reverse *tablero*))) "ganar" jugador "numero")) 
                    (and (setq el "second") (setq accum (append (list (nth col (nth row (reverse *tablero*)))))))
                  )
                )
              )
              (if (= col 2) (and (setq row (+ row 1)) (setq col 0)) (setq col (+ col 1)))
            )
          ) 
      ) 
    )
    (if (null accum)
      (return-from casilla-destino nil)
      (return-from casilla-destino (list (first accum) el row))
    )
    
  )
)

(defun casillas-jugador (jugador)
"Regresa una lista con las casillas ocupadas del jugador en turno"
  (let (
    (accum '())
  )
    (if (string-equal jugador "X")
      (progn 
        (loop for e in *fichas_ocupadas_jugador_1* do
          (setq accum (append (list (first e)) accum))
        )
      )
      (progn 
        (loop for e in *fichas_ocupadas_jugador_2* do
          (setq accum (append (list (first e)) accum))
        )
      )
    )
    (return-from casillas-jugador (reverse accum) )
  )
)
;;;=======================================================================================
;;  FASE3 ()  
;;  ELIGE_HEURISTICA_FASE_2 (jugador)
;;  FASE2()
;;;=======================================================================================
(defun Fase3()
  nil
)
(defun elige_heuristica_fase_2 (jugador)
)

(defun Fase2 ()
  (if (and (= (length *fichas_disponibles_jugador_1*) 0) (= (length *fichas_disponibles_jugador_2*) 0))
    (progn 
        (princ "Fase 2 del Juego: ")
        (format t "~&")
        T
    )
    nil
  )
)
;;;=======================================================================================
;;  CHECK-PLAYER (prioridad jugador)  
;;  Revisa si el jugador corresopnde al tipo de prioridad elegida
;;  CHECK-VAR-TYPE (var prioridad jugador tipo)
;;  Revisa si la casilla esta marcada con una X o una O o si es numérica
;;;=======================================================================================
(defun check-player (prioridad jugador)
  (if (string-equal prioridad "ganar")
    (return-from check-player jugador)
    (if (string-equal jugador "X") "O" "X")
  )
)
(defun check-var-type (var prioridad jugador tipo)
  (cond 
    ((string-equal tipo "numero")
      (if (typep var 'number) T nil)
    )
    ((string-equal tipo "string")
      (if (typep var 'string) (string-equal var (check-player prioridad jugador)) nil)
    )
  )
)

(defun check-other-p-row (jugador prioridad)
"Revisa si hay filas del oponente con riesgo de formar una línea de 3 casillas. Si no hay, entonces regresa
 la primera casilla que encuentre del oponente"  
  (let 
    (
      (row 0)
      (col 0)
      (casilla nil)
    )
    (loop named cwr for r in (reverse *tablero*) do
      (cond 
        ((and (check-var-type (first r) prioridad jugador "numero") (check-var-type (second r) prioridad jugador "string") (check-var-type (third r) prioridad jugador "string")) 
          (return-from cwr (setq casilla (+ (first r) 1)) )
        )
        ((and (check-var-type (first r) prioridad jugador "string") (check-var-type (second r) prioridad jugador "numero") (check-var-type (third r) prioridad jugador "string"))
          (return-from cwr (setq casilla  (- (second r) 1) ))
        )
        ((and (check-var-type (first r) prioridad jugador "string") (check-var-type (second r) prioridad jugador "string") (check-var-type (third r) prioridad jugador "numero"))
          (return-from cwr (setq casilla  (- (third r) 1) ))
        )
      )
    )
    (if (null casilla)
      (loop named cor for i from 1 to 24 do
        (cond 
          ((check-var-type (nth col (nth row (reverse *tablero*))) prioridad jugador "string") (and (setq casilla i) (return-from cor casilla)))
        )
        (if (= col 2) (and (setq row (+ row 1)) (setq col 0)) (setq col (+ col 1)))
      )
      (return-from check-other-p-row casilla)
    )
    (return-from check-other-p-row casilla)
  )
)

(defun check-other-p-col-diag (jugador prioridad ctype elemento casilla)
"Revisa si hay columnas del oponente con riesgo de formar una línea de 3 casillas. Si no hay, entonces regresa
 la primera casilla que encuentre del oponente."
  (let 
    (
      (row 0)
      (col 0)
      (casilla_destino nil)
      (cols '((1 10 22)(4 11 19)(7 12 16)(2 5 8)(9 13 18)(6 14 21)(3 15 24)(7 20 23)))
    )
    ;; casilla es de tipo (19 X X)
    (if (string-equal ctype "col") 
      (progn 
        (loop named cwc for c in cols do
          (cond 
            ((and 
              (string-equal elemento "first") 
              (= (first c) (first casilla))
              (check-var-type (second casilla) prioridad jugador "string")
              (check-var-type (third casilla) prioridad jugador "string"))
              (return-from cwc (setq casilla_destino (second c)))
            )
            ((and 
              (string-equal elemento "second") 
              (= (second c) (second casilla))
              (check-var-type (first casilla) prioridad jugador "string")
              (check-var-type (third casilla) prioridad jugador "string"))
              (return-from cwc (setq casilla_destino (first c)))
            )
            ((and 
              (string-equal elemento "third") 
              (= (third c) (third casilla))
              (check-var-type (first casilla) prioridad jugador "string")
              (check-var-type (second casilla) prioridad jugador "string"))
              (return-from cwc (setq casilla_destino (second c))))
            (T nil)
          )
        )
      )
      nil
    )
    (if (Null casilla_destino)
      (loop for i from 1 to 24 do
        (cond 
          ((check-var-type (nth col (nth row (reverse *tablero*))) prioridad jugador "string") (setq casilla_destino i))
        )
        (if (= col 2) (and (setq row (+ row 1)) (setq col 0)) (setq col (+ col 1)))
      )
      (return-from check-other-p-col-diag casilla_destino)
    )
  )
)
(defun check-winning-row (jugador prioridad)
"Revisa si hay filas potenciales del jugador en turno que puedan formar una fila con tres casillas del mismo jugador.
Si la prioridad es bloquear, entonces regresa una lista con las filas potenciales que se pueden bloquear del oponente "
  (let 
    (
    (row 0)
    (casilla nil)
    (res nil)
    )
    (loop named cwr for r in (reverse *tablero*) do
      (cond 
        ((and (check-var-type (first r) prioridad jugador "numero") (check-var-type (second r) prioridad jugador "string") (check-var-type (third r) prioridad jugador "string")) 
          (return-from cwr (setq res (actualiza-casillas jugador (list r) "first" (list row row row)) ))
        )
        ((and (check-var-type (first r) prioridad jugador "string") (check-var-type (second r) prioridad jugador "numero") (check-var-type (third r) prioridad jugador "string"))
          (return-from cwr  (setq res (actualiza-casillas jugador (list r) "second" (list row row row)) ))
        )
        ((and (check-var-type (first r) prioridad jugador "string") (check-var-type (second r) prioridad jugador "string") (check-var-type (third r) prioridad jugador "numero"))
          (return-from cwr  (setq res (actualiza-casillas jugador (list r) "third" (list row row row)) ))
        )
      )
      (setq row (+ row 1))
    )
    (if (and (string-equal prioridad "ganar") res)
      (progn 
        (setq casilla (check-other-p-row jugador "bloquear"))
        (eliminar-ficha jugador casilla)
        res
      )
      res
    )
  )
)
(defun get-rec (num1 num2 num3)
"Regresa una lista con las posiciones numéricas o símbolos del tablero actual y la fila correspondiente del jugador en turno"
  (let (
      (accum-aux '())
      (accum-rows '())
      (row 0)
      (col 0)
    )
    ;; OUTPUT (17 20 "X") (2 1 0))
    (loop for i from 0 to 23 do
      (cond
        ((= i num1) (and (setq accum-aux (append (list (nth col (nth row (reverse *tablero*)))) accum-aux)) (setq accum-rows (append (list row) accum-rows))))
        ((= i num2) (and (setq accum-aux (append (list (nth col (nth row (reverse *tablero*)))) accum-aux)) (setq accum-rows (append (list row) accum-rows))))
        ((= i num3) (and (setq accum-aux (append (list (nth col (nth row (reverse *tablero*)))) accum-aux)) (setq accum-rows (append (list row) accum-rows))))
      )
      (if (> row 7) (setq row 0) (if (= col 2) (setq row (+ row 1))))
      (if (= col 2) (setq col 0) (setq col (+ col 1))) 
    )
    (return-from get-rec (list (reverse accum-aux) (reverse accum-rows) ))
  )
)

(defun check-other-recs (accum jugador)
"Regresa una lista con las posiciones numéricas o símbolos del tablero actual y la fila correspondiente del oponente"
  (let 
    (
      (el nil)
      (res2 nil)
    )
    (loop named cwc_2 for r in accum do
      (cond 
        ((and (check-var-type (first (first r)) "bloquear" jugador "numero") (check-var-type (second (first r)) "bloquear" jugador "string") (check-var-type (third (first r)) "bloquear" jugador "string") ) 
          (return-from cwc_2  (and (setq el "first") (setq res2 r) ) )
        )
        ((and (check-var-type (first (first r)) "bloquear" jugador "string") (check-var-type (second (first r)) "bloquear" jugador "numero") (check-var-type (third (first r)) "bloquear" jugador "string"))
          (return-from cwc_2  (and (setq el "second") (setq res2 r) ) )
        )
        ((and (check-var-type (first (first r)) "bloquear" jugador "string") (check-var-type (second (first r)) "bloquear" jugador "string") (check-var-type (third (first r)) "bloquear" jugador "numero"))
          (return-from cwc_2  (and (setq el "third") (setq res2 r) ) )
        )
      )
    )
    (return-from check-other-recs (list el res2))
  )
)
(defun check-win-recs (accum prioridad jugador)
  (let 
    (
      (el nil)
      (res nil)
    )
    (loop named cwc for r in accum do
      (cond 
        ((and (check-var-type (first (first r)) prioridad jugador "numero") (check-var-type (second (first r)) prioridad jugador "string") (check-var-type (third (first r)) prioridad jugador "string") ) 
          (return-from cwc  (and (setq el "first") (setq res (actualiza-casillas jugador r "first"  (second r))) ))
        )
        ((and (check-var-type (first (first r)) prioridad jugador "string") (check-var-type (second (first r)) prioridad jugador "numero") (check-var-type (third (first r)) prioridad jugador "string"))
          (return-from cwc  (and (setq el "second") (setq res (actualiza-casillas jugador r "second" (second r)) ) ))
        )
        ((and (check-var-type (first (first r)) prioridad jugador "string") (check-var-type (second (first r)) prioridad jugador "string") (check-var-type (third (first r)) prioridad jugador "numero"))
          (return-from cwc  (and (setq el "third") (setq res (actualiza-casillas jugador r "third" (second r)) ) ))
        )
      )
    )
    (return-from check-win-recs (list el res))
  )
)
(defun apply-rec (rectype)
  (let (
    (accum '())
    (res nil)
  )
    (cond
      ((string-equal rectype "col")
        (progn 
          (setq accum (append (list (get-rec 0 9 21)) accum) )
          (setq accum (append (list (get-rec 3 10 18)) accum) )
          (setq accum (append (list (get-rec 6 11 15)) accum) )
          (setq accum (append (list (get-rec 1  4  7)) accum) )
          (setq accum (append (list (get-rec 8 12 17)) accum) )
          (setq accum (append (list (get-rec 5 13 20)) accum) )
          (setq accum (append (list (get-rec 2 14 23)) accum) )
          (setq accum (append (list (get-rec 16 19 22)) accum) )
        )
      )
    )
    (return-from apply-rec accum)
  )
)
(defun check-winning-column (jugador prioridad)
"Revisa si hay columnas potenciales del jugador en turno que puedan formar una columna con tres casillas del mismo jugador.
Si la prioridad es bloquear, entonces regresa una lista con las columnas potenciales que se pueden bloquear del oponente "
(let* 
  (
   (accum (apply-rec "col"))
   (next_col 0)
   (row 0)
   (col 0)
   (tries 0)
   (filter2 '())
   (casilla nil)
   (win_e (check-win-recs accum prioridad jugador) )
   (res2 nil)
   (types '("col"))
   (res (second win_e) )
   (el (first win_e))
  )
  (if (and (string-equal prioridad "ganar") res)
      (progn 
        (loop named test while (null res2) do
          (setf res2 (second (check-other-recs accum jugador)))
          (if (null res2)
            (return-from test res2)
          )
        )
        (if (null res2)
          (progn 
            (setq casilla (check-other-p-row jugador "bloquear") )
            (if (null casilla) res (eliminar-ficha jugador casilla))
          )
          (progn
            (setq casilla (check-other-p-col-diag jugador "bloquear" (car types) el (first res2)))
            (eliminar-ficha jugador casilla)
          )
        )
        res
      )
      res
    )
  )
)

(defun random_move (jugador)
"Movimiento aleatorio realizado por el jugador artificial y realizado cuando la prioridad no es ganar ni bloquear "
  (let (
    (row 0)
    (col 0)
  )
  (loop for e in *tablero* do
    (loop for c in e do
      (if (numberp c) (progn (asignar-ficha jugador c) (setf (nth col (nth row *tablero*)) jugador) (return-from random_move (list T c)))
      )
      (if (= col 2) (and (setq row (+ row 1)) (setq col 0)) (setq col (+ col 1))
      )
    )
  )
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

(defun check-priority (op jugador)
"Predicado. Valida  un estado el tipo de prioridad a escoger"
    (let (
		  (operador (first op))
		)
		(case operador
	 	(:row_win (check-winning-row jugador "ganar"))
		(:column_win (check-winning-column jugador "ganar"))
		(:row_block (check-winning-row jugador "bloquear"))
		(:column_block (check-winning-column jugador "bloquear"))
    (:random_move (random_move jugador))
	    (nil "invalid"))
	)  
)

(defun niveles_de_prioridad (res ops jugador accum)
"Genera un loop hasta que la respuesta sea el resultado de aplicar prioridad de
 ganar, bloquear o aleatoria"
  (let (
        (res (check-priority ops jugador))
      )
      (if (first res )
          (return-from niveles_de_prioridad (second res) )
          (niveles_de_prioridad res (cdr ops) jugador accum)
      )
  )
)

(defun Fase1 (jugador)
  (let (
    (accum '())
    (resp nil)
    (resp2 nil)
  )
  (setq resp2 (niveles_de_prioridad resp *ops* jugador accum))
  (princ "Jugador O eligió: ")
  (format t "~D " resp2)
  (format t "~&")
  resp2 
  )
)

(defun FinDeJuego ( &optional terminated jugador)
"Revisa el número total de casillas que ya han sido ocupadas o si algún jugador ganó la partida"
  (if (or (<= (length *fichas_ocupadas_jugador_1*) 3)
                     (<= (length *fichas_ocupadas_jugador_2*) 3)
      )
          (return-from FinDeJuego T)
          (return-from FinDeJuego nil)
  )
)

;;;=======================================================================================
;;  MODEL (input)  
;;      Lee el input ingresado por el jugador humano. Este argumento se utiliza en la
;;       función nega-max.
;;;=======================================================================================
(defun model (input jugador)
"Revisa si la partida se encuentra en Fase 1 o en Fase 2"
    (if (Fase2) (elige_heuristica_fase_2 jugador) (Fase1 jugador))
)

;;;=======================================================================================
;;  USERINPUT ()  
;;      Lee y escribe la opción dada por el jugador humano en la fase 1
;;  USERINPUTF2 ()  
;;      Lee y escribe la opción dada por el jugador humano en la fase 2, para la casilla
;;      origen como para la casilla destino.
;;;=======================================================================================
(defun UserInput()
  (terpri)
  (princ "Ingrese un número de casilla valido: ")
  (setq respuesta (read))
  (princ "Jugador X eligió: ")
  (write respuesta)
)

(defun UserInputF2()
  (terpri)
  (let (
          (res (casillas-jugador "X"))
          (res2 nil)
        )
  (if (null res) (return-from UserInputF2 nil))
  (format t "Seleccione la casilla a mover: ~D" )
  (format t "~&")
  (setq respuesta (read))
  (if (checa-casilla-origen respuesta (casillas-jugador "X") )
    (progn 
      (princ "Jugador X eligió: ")
      (write respuesta)
      (format t "~&")
        (if (null res)
          (return-from UserInputF2 "FinDeJuego")
          (progn 
            (setq res2 (casilla-destino respuesta "X"))
            (format t "Seleccione la casilla destino: ~D" (first res2))
            (format t "~&")
            (setq respuesta2 (read))
            (if (checa-casilla-destino respuesta2 (casilla-destino respuesta "X") )
              (progn 
                (mover-ficha "X" respuesta respuesta2) 
                (princ "Jugador X movió su casilla a la posición: ")
                (write respuesta2)
                (format t "~&")
                (return-from UserInputF2 respuesta2)
              )
              (progn 
                (princ "Casilla Invalida")
                (UserInputF2)
              )
            )
          )
        )
    )
    (progn 
      (princ "Casilla Invalida")
       (UserInputF2)
    )
  )
  )
)


;;;=======================================================================================
;;  SET-BOARD ( & input)  
;;      input - La casilla elegida por el jugador humano. Puede ser un número del 1 al 24
;;;=======================================================================================
(defun set-board ( &optional input jugador)
"Crea y genera el tablero de 3 x 3, en donde cada elemento de esta matriz es una matriz 
  de 3 x 3. Utilizando el método format se muestra la interfaz al usuario jugador y se
  indican los movimientos el jugador humano y el jugador artificial han realizado durante la
  partida"
	(let ((board nil)
		  (accum nil)
      (accum2 nil)
      (p_id 0)
      (row 0)
      (column 0)
      (rnum 0)
      (cnum 0)
      (rnum2 0)
      (cnum2 0)
      (res input)
		)
    (if (= *turno* 1)
      (progn 
        (create-fichas)
        (loop for i from 1 to 24 do
          (setq accum (cons i accum))
          (if (= (mod i 3) 0) 
            (progn 
              (setf *tablero* (push (reverse accum) *tablero*))
              (setq accum '())
            )
          )
        )
      )
      (progn
        (loop for r in (reverse *tablero*) do
          (loop for c in r do
            (cond 
              ((and (numberp res) (numberp c) (= res c)) 
                (setf (nth cnum (nth rnum  (reverse *tablero*))) jugador))
              (T )
            )
            (incf cnum)
          )
          (incf rnum)
          (setf cnum 0)
        )
      )
    )
    (setq board *tablero*)
    (loop for r in (reverse board) do
      (loop for c in r do
        (setq column (+ column 1))
        (cond 
          ((= row 0) (if (< column 3)
            (format t "|  ~D | -- | -- " c) 
            (progn 
                 (format t "|  ~D |  " c)
                 (format t "~&") 
                 (dotimes (i 7)
                   (format t "  -- ")
                 )
                 (format t "~&")))
            )
            ((= row 7) (if (< column 3)
            (format t "| ~D | -- | -- " c) 
            (progn 
                 (format t "| ~D |  " c)
                 (format t "~&") 
                 (dotimes (i 7)
                   (format t "  -- ")
                 )
                 (format t "~&")))
            )
          ((= row 1) 
            (cond 
              ((= column 1) (format t "|  | |  ~D | -- " c))
              ((= column 2) (format t "|  ~D | -- " c))
              ((= column 3) (progn (format t "|  ~D |  | |" c) 
                                   (format t "~&") 
                                   (dotimes (i 7)
                                    (format t "  -- " c)
                                   )
                                   (format t "~&")))
            )
          )
          ((= row 6)
            (cond 
              ((= column 1) (format t "|  | | ~D | -- " c))
              ((= column 2) (format t "| ~D | -- " c))
              ((= column 3) (progn (format t "| ~D |  | |" c) 
                                   (format t "~&") 
                                   (dotimes (i 7)
                                    (format t "  -- " c)
                                   )
                                   (format t "~&")))
            )
          )
          ((= row 2)
            (cond 
              ((= column 1) (format t "|  | |  | |  ~D " c))
              ((= column 2) (format t "|  ~D | " c))
              ((= column 3) (progn (format t " ~D |  | |  | |  " c)
                                  (format t "~&") 
                                  (dotimes (i 7)
                                    (format t "  -- " c)
                                  )
                                  (format t "~&")))
            )
          )
          ((= row 5)
            (cond 
              ((= column 1) (format t "|  | |  | | ~D " c))
              ((= column 2) (format t "| ~D | " c))
              ((= column 3) (progn (format t "~D |  | |  | |  " c)
                                  (format t "~&") 
                                  (dotimes (i 7)
                                    (format t "  -- " c)
                                  )
                                  (format t "~&")))
            )
          )
          ((or (= row 3) (= row 4))
            (cond 
              ((= column 1) (format t "| ~D " c))
              ((= column 2) (format t "| ~D " c))
              ((= column 3) (if (= row 3) (format t "| ~D |  + " c) 
                (progn 
                  (format t "| ~D | " c)
                  (format t "~&") 
                  (dotimes (i 7)
                    (format t "  -- " c)
                  )
                  (format t "~&"))))
            )
          )
          
        )
      )
      (setq column 0)
      (setq row (+ row 1))
    )
    (if (Fase2) 
      (progn 
        (if (evenp *turno*)
          (progn  
            (setq *turno* (+ *turno* 1))
            (set-board (model res "O") "O")
          )
          (progn 
            (setq res (UserInputF2))
            (format t "~&")
            (if 
              (and 
                (numberp res) 
                (>= res 1) 
                (<= res 24)
                (setq *turno* (+ *turno* 1))
              ) (set-board res "X") (return-from set-board "FIN DEL JUEGO"))
          )
        )
      )
      (progn 
        (if (evenp *turno*)
          (progn  
            (setq *turno* (+ *turno* 1))
            (set-board (model res "O") "O")
          )
          (progn 
            (setq res (UserInput))
            (format t "~&")
            (if 
              (and 
                (numberp res) 
                (> res 0) 
                (<= res 24)
                (setq *casillas_ocupadas* (append (list res) *casillas_ocupadas*))
                (asignar-ficha "X" res)
                (setq *turno* (+ *turno* 1))
              ) (set-board res "X") (return-from set-board "FIN DEL JUEGO"))
          )
        )
      )
    )
	)
)
(set-board)
(defpackage :annie
  (:use :common-lisp :ltk :fits :tktable :metodos-numericos)
  (:shadowing-import-from :ltk :bbox :scale :font-metrics :image)
  (:export #:annie))

(in-package :annie)

;(eval-when (:load-toplevel)
 ; (setf ltk:*init-wish-hook* (append ltk:*init-wish-hook*
  ;                                   (list (lambda ()
   ;                                          (ltk:format-wish "source tktable/tkTable.tcl")
;					     (ltk:format-wish "package require Tktable"))))))

(defparameter *nombre-programa* "Annie")
(defparameter *subnombre-programa* "AN�lisis Num�rico de la Informaci�n Espectral")
(defparameter *version* "1.3.0")
(defparameter *a�o* 2017)
(defparameter *autor* "Luis G. L�pez")
(defparameter *agradecimientos* "

Agradecimientos:
       * a Peter Herth, por unir m�gicamente Common Lisp y Tcl/Tk.
       * a Jason Miller, por su soluci�n a un arcano gr�fico que me resultaba perfectamente inaccesible.
       * a A.J. Pickles et al. (1998), por sus magn�ficos espectros de referencia.
       * a Regina Coluzzi (1993-1999), por su exquisita y minuciosa lista de l�neas de absorci�n generadas por cada elemento qu�mico.
       * a Peter Spjuth y Yaroslav Schekin, por su m�gica librer�a para la creaci�n de PDFs.
       * a Diego Guberman, por sus generosas y precisas sugerencias.")
(defparameter *creditos* (concatenate 'string
				      (format nil "~a:~% *** ~a *** ~%~%  v~a (~a)       -       Autor:  ~a"
					      *nombre-programa* *subnombre-programa* *version* *a�o* *autor*)
				      *agradecimientos*))

(defparameter *mensaje-salir*
  (format nil "No quiero resultar pesada, pero... �Grabaste ya el trabajo realizado? Mir� que despu�s ser� demasiado tarde para l�grimas...  ~%~%~40t Atte., Annie."))
(defparameter *mensaje-cerrar*
  (format nil "No quiero sonar impertinente, pero... �Grabaste ya el trabajo realizado en esta ventana? Mir� que despu�s ser� demasiado tarde para l�grimas...  ~%~%~40t Atte., Annie."))
(defparameter *mensaje-calibracion*
  (format nil "Perdoname, pero, �no ser�a mejor realizar la calibraci�n sobre el perfil principal de la ventana?  ~%~%~40t Atte., Annie."))
(defparameter *mensaje-calibracion-2*
  (format nil "Perdoname, pero, �no ser�a mejor encontrar antes al menos  dos baricentros del perfil principal de la ventana?  ~%~%~40t Atte., Annie."))
(defparameter *mensaje-copiar*
  (format nil "Ser� curiosa, �para qu� querr�as copiar un perfil a�n no calibrado? Honestamente, yo no le encuentro mucho sentido. ~%~%~40t Atte., Annie."))
(defparameter *mensaje-pegar*
  (format nil "Disculpame, �para qu� querr�as pegar un perfil junto a uno que a�n no ha sido calibrado? Honestamente, yo no le encuentro mucho sentido. ~%~%~40t Atte., Annie."))
(defparameter *mensaje-borrar*
  (format nil "No quiero sonar impertinente, pero... �Grabaste ya el trabajo realizado en esta ventana? Mir� que despu�s ser� demasiado tarde para l�grimas...  ~%~%~40t Atte., Annie."))

(defparameter *catalogo-elementos* nil)
(defparameter *ventana-elementos* nil)
(defparameter *directorio-inicio* ".")

(defclass perfil ()
  ((1-d :initarg :1-d :accessor 1-d)
   (longitud :initarg :longitud :accessor longitud)
   (minimo :initarg :minimo :accessor minimo)
   (maximo :initarg :maximo :accessor maximo)
   (nombre :initarg :nombre :accessor nombre)
   (etiqueta :initarg :etiqueta :accessor etiqueta :initform nil)
   (linea-etiqueta :initarg :linea-etiqueta :accessor linea-etiqueta)
   (color  :initarg :color  :accessor color :initform "#000000")
   (pattern  :initarg :pattern  :accessor pattern :initform "")
   (ax     :initarg :ax :accessor ax :initform nil)
   (bx     :initarg :bx :accessor bx)
   (ai     :initarg :ai :accessor ai :initform nil)
   (bi     :initarg :bi :accessor bi)
   (ac     :initarg :ac :accessor ac :initform nil)
   (bc     :initarg :bc :accessor bc :initform nil)
   (zx     :initarg :zx :accessor zx :initform 1)
   (zy     :initarg :zy :accessor zy :initform 1)
   (desp-y :initarg :desp-y :accessor desp-y :initform 0)
   (ppal   :initarg :ppal :accessor ppal)
   (lista-coords :initarg :lista-coords :accessor lista-coords)
   (linea-perfil :initarg :linea-perfil :accessor linea-perfil)
   (banderas :initarg :banderas :accessor banderas :initform nil)
   (eje-x    :initarg :eje-x :accessor eje-x :initform nil)
   (eje-y    :initarg :eje-y :accessor eje-y :initform nil)))

(defclass bandera ()
  ((perfil :initarg :perfil :accessor perfil)
   (linea :initarg :linea :accessor linea)
   (etiqueta :initarg :etiqueta :accessor etiqueta)
   (baricentro-x :initarg :baricentro-x :accessor baricentro-x)
   (texto-c    :initarg :texto-c :accessor texto-c)
   (texto    :initarg :texto :accessor texto :initform "")
   (pos-x0    :initarg :pos-x0 :accessor pos-x0)
   (pos-y0    :initarg :pos-y0 :accessor pos-y0)
   (pos-x1    :initarg :pos-x1 :accessor pos-x1)
   (pos-y1    :initarg :pos-y1 :accessor pos-y1)))

(defclass eje ()
  ((perfil :initarg :perfil :accessor perfil)
   (tiks :initarg :ticks :accessor ticks)
   (linea :initarg :linea :accessor linea)
   (lineas-ticks :initarg :lineas-ticks :accessor lineas-ticks)
   (etiquetas :initarg :etiquetas :accessor etiquetas)))

(defparameter *perfil-copiado* nil)

(defun copiar-perfil-objeto (perfil)
  (let ((nuevo (make-instance 'perfil :nombre (nombre perfil) :1-d (copy-seq (1-d perfil)) :minimo (minimo perfil) :maximo (maximo perfil)
			      :color (color perfil) :pattern (pattern perfil) :ax (ax perfil) :bx (bx perfil) :ai (ai perfil) :bi (bi perfil)
			      :ac (ac perfil) :bc (bc perfil) :zx (zx perfil) :zy (zy perfil)
			      :longitud (longitud perfil) :lista-coords (copy-list (lista-coords perfil)))))
    (setf (banderas nuevo) (copiar-banderas perfil nuevo))
    nuevo))

(defun copiar-banderas (viejo nuevo)
  (let (nuevas)
    (dolist (b (banderas viejo))
      (push (make-instance 'bandera :perfil nuevo :baricentro-x (baricentro-x b) :texto (texto b)
			   :pos-x0 (pos-x0 b) :pos-y0 (pos-y0 b) :pos-x1 (pos-x1 b) :pos-y1 (pos-y1 b))
	    nuevas))
    nuevas))

(defun ppal? (perfil) (eq perfil (ppal perfil)))

(defun baricentro-viejo (perfil x0 x1)   ;;; REVISAR
  (loop for x from (round (min x0 x1)) upto (round (max x0 x1))
     and potencia = -2
     for valor = (expt (svref (1-d perfil) x) potencia)
     summing (* x valor) into suma-ponderada
     summing valor into suma-intensidades
     finally (return (float (/ suma-ponderada suma-intensidades)))))

(defun baricentro (perfil x0 x1)
  (let* ((x-min (round (min x0 x1)))
	 (x-max (round (max x0 x1)))
	 (n (- x-max x-min -1))
	 (datos-xy (make-array (list n 2) :initial-element 0)))
    (dotimes (i n)
       (setf (aref datos-xy i 0) (+ i x-min)
	     (aref datos-xy i 1) (float (svref (1-d perfil) (+ i x-min)) 1l0)))
    (ajuste-gaussiano datos-xy)))

(defun x->xc (perfil x) (+ (* (ax perfil) (if (ppal? perfil) x (x1->x2 perfil (ppal perfil) x)))
			   (bx perfil)))
(defun i->yc (perfil i) (+ (* (ai perfil) i) (bi perfil)
			   (desp-y perfil)))
(defun xc->x (perfil xc) (if (ppal? perfil)
			     (- (/ xc (ax perfil)) (/ (bx perfil) (ax perfil)))
			     (x1->x2 (ppal perfil) perfil (- (/ xc (ax perfil)) (/ (bx perfil) (ax perfil))))))
(defun yc->i (perfil yc) (- (/ (- yc (desp-y perfil)) (ai perfil))
			    (/ (bi perfil) (ai perfil))))
(defun xc->i (perfil xc)
  (let ((x (round (xc->x perfil xc))))
    (if (< -1 x (longitud perfil))
	(svref (1-d perfil) x))))
(defun x->i (perfil x)
  (let ((x (round x)))
    (if (< -1 x (longitud perfil))
	(svref (1-d perfil) x))))
(defun xsc->xc (canvas xsc) (canvasx canvas xsc))
(defun ysc->yc (canvas ysc) (canvasy canvas ysc))

(defun x->l (perfil x) (+ (* (ac perfil) x) (bc perfil)))
(defun xc->l (perfil xc) (x->l perfil (xc->x perfil xc)))
(defun l->x (perfil l) (- (/ l (ac perfil)) (/ (bc perfil) (ac perfil))))
(defun x1->x2 (perfil-1 perfil-2 x) (l->x perfil-2 (x->l perfil-1 x)))

(defun posicion-canvas (perfil x y)
  (list (x->xc perfil x)
	(i->yc perfil y)))
(defun generar-coordenadas! (perfil recortar?)
  (dotimes (i (longitud perfil))
    (setf (nth (* 2 i)       (lista-coords perfil)) (x->xc perfil i)
	  (nth (+ (* 2 i) 1) (lista-coords perfil)) (i->yc perfil (svref (1-d perfil) i))))
  (if recortar?
      (subseq (lista-coords perfil) (* 2 (ceiling (max 0 (x1->x2 (ppal perfil) perfil 0))))
	      (* 2 (floor (min (1- (longitud perfil)) (x1->x2 (ppal perfil) perfil (1- (longitud (ppal perfil))))))))
      (lista-coords perfil)))

(defun x (pos) (first pos))
(defun y (pos) (second pos))

(defun crear-coeficientes-transformacion! (perfil lienzo h h-eje-x h-eje-y &optional ancho alto)
  (let* ((l (longitud perfil))
	 (ma (maximo perfil))
	 (mi (minimo perfil))
	 (zx (zx perfil))
	 (zy (zy perfil))
	 (ww (if ancho ancho (window-width lienzo)))
	 (wh (if alto alto (window-height lienzo)))
	 (x0 (if (ax perfil) (xc->x perfil (+ (xsc->xc lienzo (/ ww 2.0)) (/ h-eje-y 2))) (/ l 2.0)))
	 (y0 (if (ai perfil) (yc->i perfil (+ (ysc->yc lienzo (/ wh 2.0)) (/ h-eje-x 2))) (/ (+ ma mi) 2.0)))
	 (auxx (if (ppal? perfil) (/ (- ww h h h-eje-y) l)))
	 (auxi (if (= mi ma) 1 (/ (- wh h h h-eje-x) (- mi ma)))))
    (setf (ax perfil) (if (ppal? perfil)
			  (* zx auxx)
			  (ax (ppal perfil)))
	  (bx perfil) (if (ppal? perfil)
			  (+ h h-eje-y (* auxx (- 1 zx) x0))
			  (bx (ppal perfil)))
	  (ai perfil) (* zy auxi)
	  (bi perfil) (+ (- h (* ma auxi))
			 (* auxi (- 1 zy) y0)))))

(defun crear-coeficientes-calibracion! (perfil x1 x2 l1 l2)
  (setf (ac perfil) (/ (- l2 l1) (- x2 x1))
	(bc perfil) (/ (- (* l1 x2) (* l2 x1)) (- x2 x1))))

(defun prompt-input-box (prompt &key (title "Input") default)
  (let* ((*exit-mainloop* nil)
	 (ok t)
	 (w (make-instance 'toplevel :title title))
	 (l (make-instance 'label :master w :text prompt))
	 (e (make-instance 'text :master w :width 30 :height 3))
	 (f (make-instance 'frame :master w))
	 (b_ok (make-instance 'button :master f :text "Ok"
			      :command (lambda ()
					 (ltk::break-mainloop))))
	 (b_cancel (make-instance 'button :master f :text "Cancelar"
				  :command (lambda ()
					     (setf ok nil)
					     (ltk::break-mainloop)))))
    (pack l :side :top :anchor :w)
    (pack e :side :top)
    (pack f :side :top :anchor :e)
    (pack b_cancel :side :right)
    (pack b_ok :side :right)
    (bind w "<Control-Return>" (lambda (event)
			 (declare (ignore event))
			 (ltk::break-mainloop)))
    (when default
      (setf (text e) (string-right-trim (string #\Newline) default)))
    (focus e)
    (mainloop)
    (withdraw w)
    (and ok
	 (concatenate 'string (string-right-trim (string #\Newline) (remove #\Return (text e)))
		      (string #\Newline) (string #\Newline)))))

(defun ventana-calibracion (n lista-baricentros)
  (labels ((entry->numero (s)
	     (read-from-string (substitute #\. #\, (text s)) nil "-")))
    (let ((*exit-mainloop* nil)
	  (ok t)
	  lista-x lista-l)
      (with-widgets
	  (toplevel top-frame :title "Calibraci�n"
		    (frame marco :pack '(:side :top) :relief :sunken)
		    (button b-ok :text "Ok" :pack '(:side :right :padx 3 :pady 5)
			    :command (lambda ()
				       (ltk::break-mainloop)))
		    (button b-cancel :text "Cancelar" :pack '(:side :right :padx 3 :pady 5)
			    :command (lambda ()
				       (setf ok nil)
				       (ltk::break-mainloop))))
	(dotimes (i n)
	  (make-instance 'label :text (format nil "x~a: " (+ i 1)) :grid (list i 0) :master marco)
	  (push (make-instance 'entry :width 10 :grid (list i 1) :master marco :text (format nil "~@[~a~]" (nth i lista-baricentros))) lista-x)
	  (make-instance 'label :text "->" :grid (list i 2) :master marco)
	  (make-instance 'label :text (format nil "lambda~a: " (+ i 1)) :grid (list i 3) :master marco)
	  (push (make-instance 'entry :width 10 :grid (list i 4) :master marco) lista-l))
	(focus (car (last lista-x)))
	(mainloop)
	(withdraw top-frame)
	(when ok
	  (let ((lista-xn (mapcar #'entry->numero lista-x))
		(lista-ln (mapcar #'entry->numero lista-l)))
	    (when (and (every #'numberp lista-xn) (every #'numberp lista-ln))
	      (values lista-xn lista-ln))))))))

(defun borrar-dibujos (perfil lienzo)
  (with-slots (linea-perfil banderas eje-x eje-y) perfil
    (itemdelete lienzo linea-perfil)
    (dolist (bandera banderas)
      (borrar-dibujos-bandera lienzo bandera))
    (borrar-eje eje-x lienzo)
    (borrar-eje eje-y lienzo)))

(defun borrar-dibujos-bandera (lienzo bandera)
  (loop for e in '(linea etiqueta texto-c) do (itemdelete lienzo (slot-value bandera e))))

(defun valor (widget)
  (read-from-string (text widget)))

(defun n->esp (numero decimales)
  (multiple-value-bind (entero decimal) (truncate numero)
    (format nil "~,,'.,:d~:[~;,~]~a" entero (plusp decimales) (subseq (format nil "~v$" decimales decimal) 2))))

(defun actualizar-perfiles-box (pesta�a)
  (with-slots (perfiles perfil-activo perfiles-box) pesta�a
    (setf (options perfiles-box) (mapcar #'(lambda (e) (format nil "~a" (nombre e))) perfiles)
	  (text perfiles-box) (nombre perfil-activo))))

(defun crear-linea-perfil! (perfil lienzo)
  (setf (linea-perfil perfil) (create-line lienzo (generar-coordenadas! perfil nil)))
  (itemconfigure lienzo (linea-perfil perfil) "fill" (color perfil)))

(defun calcular-ticks (min max n)
  (if (= min max)
      (list min)
      (let* ((lista-bonita '((1.5 . 1) (2.3 . 2) (4 . 2.5) (7 . 5) (11 . 10)))
	     (s (/ (- max min) n))
	     (k (floor (log s 10)))
	     (m (/ s (expt 10 k)))
	     (step (* (cdr (find-if #'(lambda (x) (> x m)) lista-bonita :key #'car))
		      (expt 10 k))))
	(loop for tick from (* (ceiling min step) step) to (* (floor max step) step) by step
	   collecting tick))))

(defwidget (this ventana-perfil) (frame)
  (perfiles nombre-archivo lienzo cursor cursor-x0 perfil-ppal perfil-activo holgura holgura-eje-x holgura-eje-y)
  ((marco-superior frame :pack (:side :top :fill :x :padx 5 :pady 5)
		   (x-label label :text "x: " :pack (:side :left))
		   (x-entry label :text "-" :width 6 :pack (:side :left) :anchor :e :relief :sunken)
   		   (lambda-label label :text "  lambda: " :pack (:side :left))
		   (lambda-entry label :text "-" :width 9 :pack (:side :left) :anchor :e :relief :sunken)
		   (intensidad-label label :text "  Intensidad: " :pack (:side :left))
		   (intensidad-entry label :text "-" :width 13 :pack (:side :left) :anchor :e :relief :sunken)
   		   (dispersion-label label :text "  Dispersi�n: " :pack (:side :left))
		   (dispersion-entry label :text "-" :width 5 :pack (:side :left) :anchor :e :relief :sunken)
		   (perfiles-box combobox :pack (:side :right :padx 1 :pady 1))
   		   (perfiles-label label :text "  Perfil activo: " :pack (:side :right)))
   (marco-derecho frame :pack (:side :right :fill :y)
		  (marco-z-y frame :pack (:side :top)
			     (marco-zoom labelframe :text "Zoom" :pack (:side :top :ipady 3 :fill :x)
					 (opti-zoom button :text "OptiZoom" :pack (:side :top :padx 5 :fill :x) :width 10)
					 (marco-zoom-x labelframe :text "X" :pack (:side :top :fill :x :padx 5)
						       (zoom-x-- button :text "-" :width 6 :pack (:side :left :padx 3))
						       (zoom-x-+ button :text "+" :width 6 :pack (:side :right :padx 3)))
					 (marco-zoom-y labelframe :text "Y" :pack (:side :top :fill :x :padx 5)
						       (marquito-1 frame :pack (:side :top :fill :x)
								   (zoom-y-- button :text "-" :width 6 :pack (:side :left :padx 3))
								   (zoom-y-+ button :text "+" :width 6 :pack (:side :right :padx 3)))
						       (marquito-2 frame :pack (:side :top :fill :x)
								   (todos?-zoom check-button :text "�todos?" :pack (:side :top :padx 3)))))
			     (marco-desplaz-y labelframe :text "Desplazar en Y" :pack (:side :top :ipady 5 :padx 5)
					      (y-- button :text "v" :pack (:side :left :pady 5 :padx 2) :width 4)
					      (y-desp spinbox :from 1 :to 20 :increment 1 :text 10 :pack (:side :left :pady 5 :padx 2) :width 3)
					      (y-+ button :text "^" :pack (:side :left :pady 5 :padx 2) :width 4))
			     (marco-ejes labelframe :text "Ejes" :pack (:side :top :ipady 3 :fill :x :padx 5)
					 (eje-x? check-button :text "Eje X" :pack (:side :top :fill :x :padx 3))
					 (eje-y? check-button :text "Eje Y" :pack (:side :top :fill :x :padx 3)))
			     (marco-etiquetas labelframe :text "Etiquetas de perfil" :pack (:side :top :ipady 3 :fill :x :padx 5)
					      (etiquetas-activo? check-button :text "Perfil activo" :pack (:side :top :fill :x :padx 3))
					      (etiquetas-todos? check-button :text "Todos" :pack (:side :top :fill :x :padx 3)))
			     (marco-recortar labelframe :text "Recortar en X" :pack (:side :top :ipady 3 :fill :x :padx 5)
					     (recortar-x-activo? check-button :text "Perfil activo" :pack (:side :top :fill :x :padx 3))
					     (recortar-x-todos? check-button :text "Todos" :pack (:side :top :fill :x :padx 3)))
			     ))
   (sc scrolled-canvas :width 500 :height 400 :pack (:side :left :fill :both :expand t)))
  (setf (value todos?-zoom) t)
  (with-slots (perfiles perfil-ppal perfil-activo lienzo cursor cursor-x0 holgura holgura-eje-x holgura-eje-y) this
    (setf lienzo (canvas sc)
	  perfil-ppal (car perfiles)
	  perfil-activo perfil-ppal
	  cursor (create-line* lienzo 0 0 0 0)
	  cursor-x0 nil
	  holgura 10.0
	  holgura-eje-x 30.0
	  holgura-eje-y 60.0)
    (let* ((delta-x-minimo 2)
	   x0 x1)
      (labels ((actualizar-zoom (zx zy dy todos?)     ;;;;; ACTUALIZAR ZOOM ;;;;;
		 (dolist (perfil perfiles)
		   (setf (zx perfil) (if zx (* (zx perfil) zx) 1))
		   (when (or todos? (eql perfil perfil-activo))
		     (setf (zy perfil) (if zy (* (zy perfil) zy) 1)
			   (desp-y perfil) (if dy (+ (desp-y perfil) (* dy (valor y-desp))) 0)))
		   (crear-coeficientes-transformacion! perfil lienzo holgura (if (value eje-x?) holgura-eje-x 0) (if (value eje-y?) holgura-eje-y 0))
		   (set-coords lienzo (linea-perfil perfil) (generar-coordenadas! perfil (or (value recortar-x-todos?)
											     (and (eq perfil perfil-activo)
												  (value recortar-x-activo?)))))
		   (dibujar-banderas! perfil lienzo)
		   (mostrar-ejes! perfil this))
		 (calc-scroll-region lienzo)))
         ;;;;; ZOOM Y DESPLAZAMIENTO EN Y ;;;;;
	(setf (command opti-zoom) (lambda () (actualizar-zoom nil nil nil t))
	      (command zoom-x-+)  (lambda () (actualizar-zoom 1.1 1 0 t))
	      (command zoom-x--)  (lambda () (actualizar-zoom 0.9 1 0 t))
	      (command zoom-y-+)  (lambda () (actualizar-zoom 1 1.1 0 (value todos?-zoom)))
	      (command zoom-y--)  (lambda () (actualizar-zoom 1 0.9 0 (value todos?-zoom)))
	      (command y-+)       (lambda () (actualizar-zoom 1 1 -1 nil))
	      (command y--)       (lambda () (actualizar-zoom 1 1 1  nil))
	;;;;; PONER Y SACAR EJES ;;;;;
	      (command eje-x?) (lambda (valor)
				 (if (= 1 valor)
				     (mostrar-eje-x! perfil-ppal this)
				     (borrar-eje (eje-x perfil-ppal) lienzo)))
	      (command eje-y?) (lambda (valor)
				 (loop for p in perfiles do
				      (if (= 1 valor)
					  (mostrar-eje-y! p this)
					  (borrar-eje (eje-y p) lienzo))))
        ;;;;; ETIQUETAS DE PERFIL ;;;;;
	      (command etiquetas-activo?) (lambda (valor)
					    (crear-etiqueta-perfil! perfil-activo lienzo (= 1 valor) (position perfil-activo perfiles)))
	      (command etiquetas-todos?) (lambda (valor)
					   (configure etiquetas-activo? "state" (if (= 1 valor) "disabled" "normal"))
					   (setf (value etiquetas-activo?) nil)
					   (dolist (perfil perfiles)
					     (crear-etiqueta-perfil! perfil lienzo (= 1 valor) (position perfil perfiles))))
        ;;;;; RECORTAR EN X ;;;;;
	      (command recortar-x-activo?) (lambda (valor)
					     (unless (ppal? perfil-activo)
					       (set-coords lienzo (linea-perfil perfil-activo) (generar-coordenadas! perfil-activo (= 1 valor)))
					       (visibilizar-banderas perfil-activo lienzo (= 0 valor))))
	      (command recortar-x-todos?) (lambda (valor)
					    (configure recortar-x-activo? "state" (if (= 1 valor) "disabled" "normal"))
					    (setf (value recortar-x-activo?) nil)
					    (dolist (perfil (cdr perfiles))
					      (set-coords lienzo (linea-perfil perfil) (generar-coordenadas! perfil (= 1 valor)))
					      (visibilizar-banderas perfil lienzo (= 0 valor)))))
	(bind lienzo "<Motion>"            ;;;;; MOVER CURSOR ;;;;;
	      (lambda (evt)
		(let ((pos-x (xsc->xc lienzo (event-x evt)))
		      (bordes (bbox lienzo)))
		  (set-coords* lienzo cursor pos-x (second bordes) pos-x (fourth bordes))
		  (setf (text x-entry) (format nil "~a" (round (xc->x perfil-activo pos-x)))
			(text lambda-entry) (if (ac perfil-activo)
						(format nil "~$" (xc->l perfil-activo pos-x)) "-") ;(n->esp (xc->l perfil-activo pos-x) 2) "-")
			(text intensidad-entry) (or (xc->i perfil-activo pos-x) "-"))))) ; (format nil "~,,'.,:d" (or (xc->i perfil-activo pos-x) "-"))))))
	(bind lienzo "<Enter>"           ;;;;; CURSOR ENTRA A LA VENTANA ;;;;;
	      (lambda (evt) (declare (ignore evt))
		      (itemconfigure lienzo cursor "state" "normal")))
	(bind lienzo "<Leave>"           ;;;;; CURSOR SALE DE LA VENTANA ;;;;;
	      (lambda (evt) (declare (ignore evt))
		      (itemconfigure lienzo cursor "state" "hidden")
		      (setf (text x-entry) "-"
			    (text lambda-entry) "-"
			    (text intensidad-entry) "-")))
	(bind lienzo "<ButtonPress-3>"     ;;;;; MARCAR REGI�N: INICIO ;;;;;
	      (lambda (evt)
		(let ((pos-x (xsc->xc lienzo (event-x evt)))
		      (bordes (bbox lienzo)))
		  (setf cursor-x0 (create-line* lienzo pos-x (second bordes) pos-x (fourth bordes))
			x0 (xc->x perfil-activo pos-x))
		  (itemconfigure lienzo cursor-x0 "fill" (color perfil-activo))
		  (itemconfigure lienzo cursor    "dash" "11")
		  (itemconfigure lienzo cursor-x0 "dash" "11"))))
	(bind lienzo "<ButtonRelease-3>"     ;;;;; MARCAR REGI�N: FINAL ;;;;;
	      (lambda (evt)
		(itemdelete lienzo cursor-x0)
		(itemconfigure lienzo cursor "dash" "")
		(setf x1 (xc->x perfil-activo (xsc->xc lienzo (event-x evt))))
		(when (> (abs (- x1 x0)) delta-x-minimo)
		  (let* ((baricentro (baricentro perfil-activo x0 x1))
			 (bandera (make-instance 'bandera :pos-x0 baricentro :pos-x1 baricentro
						 :pos-y0 (yc->i perfil-activo (+ (i->yc perfil-activo (x->i perfil-activo baricentro)) 10))
						 :pos-y1 (yc->i perfil-activo (+ (i->yc perfil-activo (x->i perfil-activo baricentro)) 50))
						 :perfil perfil-activo :baricentro-x baricentro)))
		    (crear-atributos-bandera! bandera lienzo)
		    (push bandera (banderas perfil-activo))))))
	(bind lienzo "<Control-1>"    ;;;;; VER ELEMENTOS QU�MICOS ;;;;;
	      (lambda (evt)
		(when (and (ac perfil-activo) *ventana-elementos*)
		  (let ((tabla (widget-path *ventana-elementos*)))
		    (format-wish "~a selection clear all" tabla)
		    (let ((l-central (xc->l perfil-activo (xsc->xc lienzo (event-x evt)))))
		      (multiple-value-bind (i-1 i-2) (filtrar-filas *catalogo-elementos* l-central (ac perfil-activo))
			(when i-1
			  (format-wish "~a selection set ~a,0 ~a,3" tabla i-1 i-2)
			  (format-wish "~a see ~a,0" tabla i-1))))))))
	(bind perfiles-box "<<ComboboxSelected>>" (lambda (event)
						    (declare (ignore event))
						    (setf perfil-activo (find (text perfiles-box) perfiles :key #'nombre :test #'string=))
						    (itemconfigure lienzo cursor "fill" (color perfil-activo))
						    (setf (value etiquetas-activo?) (etiqueta perfil-activo))
						    (if cursor-x0
							(itemconfigure lienzo cursor-x0 "fill" (color perfil-activo)))
						    (if (ac perfil-activo)
							(setf (text dispersion-entry) (format nil "~$" (ac perfil-activo))))))
	(actualizar-perfiles-box this)
	(when (ac perfil-activo)
	  (setf (text dispersion-entry) (format nil "~$" (ac perfil-activo))))
	(itemconfigure lienzo cursor "fill" (color perfil-activo))
	(dolist (perfil perfiles)
	  (unless (ax perfil)
	    (crear-coeficientes-transformacion! perfil lienzo holgura 0 0 400 400))
	  (crear-linea-perfil! perfil lienzo)
	  (dolist (bandera (banderas perfil))
	    (crear-atributos-bandera! bandera lienzo)))))))

(defun crear-etiqueta-perfil! (perfil lienzo crear? n)
  (with-slots (ppal etiqueta linea-etiqueta color pattern) perfil
    (when etiqueta
      (itemdelete lienzo etiqueta)
      (itemdelete lienzo linea-etiqueta)
      (setf etiqueta nil
	    linea-etiqueta nil))
    (when crear?
      (let* ((largo 35)
	     (x (- (x->xc ppal (longitud ppal)) largo))
	     (y (+ (i->yc perfil (maximo perfil)) (* n 15))))
	(setf etiqueta (create-text lienzo x y (nombre perfil))
	      linea-etiqueta (create-line* lienzo (+ x 5) y (+ x largo) y))
	(itemconfigure lienzo etiqueta "fill" color)
	(itemconfigure lienzo etiqueta "anchor" "e")
	(itemconfigure lienzo linea-etiqueta "fill" color)
	(itemconfigure lienzo linea-etiqueta "dash" pattern)
	(itembind lienzo etiqueta "<B1-Motion>"    ;;;;; MOVER ETIQUETA Y L�NEA;;;;;
		  (lambda (evt)
		    (set-coords* lienzo etiqueta (xsc->xc lienzo (event-x evt)) (ysc->yc lienzo (event-y evt)))
		    (dibujar-linea-bandera lienzo linea-etiqueta (+ 5 (xsc->xc lienzo (event-x evt))) (ysc->yc lienzo (event-y evt))
					   (+ largo (xsc->xc lienzo (event-x evt))) (ysc->yc lienzo (event-y evt)))))))))

(defun mostrar-eje-x! (perfil tab)
  (with-slots (lienzo eje-x?) tab
    (with-slots (eje-x minimo longitud) perfil
      (setf eje-x (when (and (value eje-x?) (ppal? perfil))
		    (borrar-eje (eje-x perfil) lienzo)
		    (let* ((x0 (x->xc perfil -10))              (y0 (+ (i->yc perfil minimo) 10))
			   (x1 (+ (x->xc perfil longitud) 10))  (y1 (+ y0 10))
			   (eje-x (make-instance 'eje :perfil perfil :ticks (calcular-ticks (if (ac perfil) (x->l perfil 0) 0)
											    (if (ac perfil) (x->l perfil (longitud perfil))
												(longitud perfil))
											    (* 5 (zx perfil)))
						 :linea (create-line* lienzo x0 y0 x1 y0))))
		      (setf (lineas-ticks eje-x)
			    (loop for tick in (ticks eje-x) collecting (create-line* lienzo (x->xc perfil (if (ac perfil) (l->x perfil tick) tick)) y0
										     (x->xc perfil (if (ac perfil) (l->x perfil tick) tick)) y1))
			    (etiquetas eje-x)
			    (loop for tick in (ticks eje-x) collecting (create-text lienzo (x->xc perfil (if (ac perfil) (l->x perfil tick) tick)) y1
										    (format nil "~,,'.,:d " tick))))
		      (configurar-eje eje-x lienzo "n")
		      eje-x))))))

(defun mostrar-eje-y! (perfil tab)
  (with-slots (lienzo eje-y?) tab
    (with-slots (eje-y minimo maximo) perfil
      (setf eje-y (when (value eje-y?)
		    (borrar-eje (eje-y perfil) lienzo)
		    (let* ((x0 (x->xc (ppal perfil) -10)) (y0  (+ (i->yc perfil minimo) 10))
			   (x1 (- x0 10))                 (y1 (- (i->yc perfil maximo) 10))
			   (eje-y (make-instance 'eje :perfil perfil :ticks (calcular-ticks (minimo perfil) (maximo perfil) (* 4 (zy perfil)))
						 :linea (if (ppal? perfil)
							    (create-line* lienzo x0 y0 x0 y1)))))
		      (setf (lineas-ticks eje-y)
			    (loop for tick in (ticks eje-y) collecting (create-line* lienzo x0 (i->yc perfil tick) x1 (i->yc perfil tick)))
			    (etiquetas eje-y)
			    (loop for tick in (ticks eje-y) collecting
				 (create-text lienzo x1 (i->yc perfil tick)
					      (format nil (if (or (= 0 tick) (<= 0.0001 tick 1000000)) "~,,'.,:d" "~,1e") tick))))
		      (configurar-eje eje-y lienzo "e")
		      eje-y))))))

(defun mostrar-ejes! (perfil tab)
  (mostrar-eje-x! perfil tab)
  (mostrar-eje-y! perfil tab))

(defun configurar-eje (eje lienzo anchor)
  (loop for e in (etiquetas eje) do
       (itemconfigure lienzo e "anchor" anchor))
  (cambiar-color-eje eje lienzo))

(defun cambiar-color-eje (eje lienzo)
  (when eje
    (with-slots (perfil linea lineas-ticks etiquetas) eje
      (loop for l in lineas-ticks do
	   (itemconfigure lienzo l "fill" (color perfil)))
      (loop for e in etiquetas do
	   (itemconfigure lienzo e "fill" (color perfil)))
      (when linea
	(itemconfigure lienzo linea "fill" (color perfil))))))

(defun borrar-eje (eje lienzo)
  (when eje
    (with-slots (linea lineas-ticks etiquetas) eje
      (when linea (itemdelete lienzo linea))
      (loop for l in lineas-ticks do (itemdelete lienzo l))
      (loop for e in etiquetas do (itemdelete lienzo e)))))

(defun dibujar-linea-bandera (lienzo linea x0 y0 x1 y1)
  (set-coords* lienzo linea x0 y0 x0 y1 x1 y1))

(defun dibujar-banderas! (perfil lienzo)
  (dolist (b (banderas perfil))
    (let ((pos0 (posicion-canvas perfil (pos-x0 b) (pos-y0 b)))
	  (pos1 (posicion-canvas perfil (pos-x1 b) (pos-y1 b))))
      (set-coords* lienzo (etiqueta b) (x pos1) (y pos1))
      (set-coords* lienzo (texto-c b)  (x pos1) (+ (y pos1) 15))
      (dibujar-linea-bandera lienzo (linea b) (x pos0) (y pos0) (x pos1) (y pos1)))))

(defun visibilizar-banderas (perfil lienzo visible?)
  (let ((estado (if visible? "normal" "hidden")))
    (dolist (b (banderas perfil))
      (loop for item in '(linea texto-c etiqueta)
	 do (if (not (<= (x1->x2 (ppal perfil) perfil 0) (baricentro-x b) (x1->x2 (ppal perfil) perfil (longitud (ppal perfil)))))
		(itemconfigure lienzo (slot-value b item) "state" estado))))))

(defun crear-atributos-bandera! (bandera lienzo)
  (let* ((perfil (perfil bandera))
	 (pos0 (posicion-canvas perfil (pos-x0 bandera) (pos-y0 bandera)))
	 (pos1 (posicion-canvas perfil (pos-x1 bandera) (pos-y1 bandera))))
    (setf (etiqueta bandera) (create-text lienzo (x pos1) (y pos1) (format nil "~a" (if (ac perfil)
											(x->l perfil (baricentro-x bandera))
											(baricentro-x bandera))))
	  (linea bandera)   (create-line* lienzo (x pos0) (y pos0) (x pos0) (y pos1) (x pos1) (y pos1))
	  (texto-c bandera) (create-text lienzo (x pos1)  (+ (y pos1) 15) (or (texto bandera) "")))
    (itemconfigure lienzo (etiqueta bandera) "anchor" (if (< (pos-y1 bandera) (pos-y0 bandera)) "n" "s"))
    (itemconfigure lienzo (texto-c bandera) "anchor" "n")
    (itemconfigure lienzo (texto-c bandera) "justify" "center")
    (itemconfigure lienzo (etiqueta bandera) "fill" (color perfil))
    (itemconfigure lienzo (linea bandera) "fill" (color perfil))
    (itemconfigure lienzo (texto-c bandera) "fill" (color perfil))
    (itembind lienzo (etiqueta bandera) "<Double-1>"   ;;;;; AGREGAR TEXTO ;;;;;
	      (lambda (evt)
		(declare (ignore evt))
		(itemconfigure lienzo (texto-c bandera) "text" (setf (texto bandera)
								     (or (prompt-input-box "Elementos o compuestos qu�micos:"
											   :title "Ingreso de datos"
											   :default (texto bandera))
									 (texto bandera))))
		(itemconfigure lienzo (texto-c bandera) "anchor" (if (< (pos-y1 bandera) (pos-y0 bandera)) "n" "s"))))
    (labels ((mover-labels ()
	       (loop for item in '(etiqueta texto-c) do
		    (itemconfigure lienzo (slot-value bandera item)
				   "anchor" (if (< (pos-y1 bandera) (pos-y0 bandera)) "n" "s")))))
      (itembind lienzo (etiqueta bandera) "<B1-Motion>"    ;;;;; MOVER ETIQUETA ;;;;;
		(lambda (evt)
		  (let ((x1 (xsc->xc lienzo (event-x evt)))
			(y1 (ysc->yc lienzo (event-y evt))))
		    (setf (pos-x1 bandera) (xc->x perfil x1)
			  (pos-y1 bandera) (yc->i perfil y1))
		    (set-coords* lienzo (etiqueta bandera) x1 y1)
		    (set-coords* lienzo (texto-c bandera)  x1 (+ y1 15))
		    (dibujar-linea-bandera lienzo (linea bandera) (x->xc perfil (pos-x0 bandera)) (i->yc perfil (pos-y0 bandera)) x1 y1))
		  (mover-labels)))
      (itembind lienzo (linea bandera) "<B1-Motion>"    ;;;;; MOVER LINEA ETIQUETA ;;;;;
		(lambda (evt)
		  (let ((x0 (xsc->xc lienzo (event-x evt)))
			(y0 (ysc->yc lienzo (event-y evt))))
		    (setf (pos-x0 bandera) (xc->x perfil x0)
			  (pos-y0 bandera) (yc->i perfil y0))
		    (dibujar-linea-bandera lienzo (linea bandera) x0 y0 (x->xc perfil (pos-x1 bandera)) (i->yc perfil (pos-y1 bandera))))
		  (mover-labels)))
      (itembind lienzo (etiqueta bandera) "<Shift-1>"  ;;;; BORRAR ETIQUETA ;;;;;
		(lambda (evt)
		  (declare (ignore evt))
		  (when (ask-yesno "�Realmente quiere borrar la etiqueta?")
		    (borrar-dibujos-bandera lienzo bandera)
		    (setf (banderas perfil) (remove bandera (banderas perfil)))))))))

(defgeneric tab-seleccionado (nb tabs))
(defmethod tab-seleccionado ((nb notebook) tabs)
  (format-wish "senddatastring [~a select]" (widget-path nb))
  (find (ltk::read-data) tabs :key #'widget-path :test #'string-equal))

(defgeneric notebook-tabs (nb))
(defmethod notebook-tabs ((nb notebook))
  (format-wish "senddatastring [~a tabs]" (widget-path nb))
  (ltk::read-data))

(defun cambiar-color (tab)
  (with-slots (lienzo perfil-activo cursor) tab
    (let ((color (choose-color :title "Elegir un color" :initialcolor (color perfil-activo))))
      (when (string-not-equal color "")
	(setf (color perfil-activo) color)
	(itemconfigure lienzo (linea-perfil perfil-activo) "fill" (color perfil-activo))
	(itemconfigure lienzo cursor "fill" (color perfil-activo))
	(when (etiqueta perfil-activo)
	  (itemconfigure lienzo (etiqueta perfil-activo) "fill" (color perfil-activo))
	  (itemconfigure lienzo (linea-etiqueta perfil-activo) "fill" (color perfil-activo)))
	(dolist (bandera (banderas perfil-activo))
	  (loop for e in '(etiqueta linea texto-c) do (itemconfigure lienzo (slot-value bandera e) "fill" color)))
	(cambiar-color-eje (eje-x perfil-activo) lienzo)
	(cambiar-color-eje (eje-y perfil-activo) lienzo)))))

(defun ventana-patterns (pattern)
  (let ((*exit-mainloop* nil)
	(ok t)
	(patron pattern))
    (labels ((pat (radio)
	       (setf patron
		     (case (value radio)
		       (1 "")
		       (2 ".")
		       (3 "-")
		       (4 "-.")
		       (5 "-..")))))
      (with-widgets
	  (toplevel top-frame :title "Dise�o de l�nea"
		    (labelframe marco :text "Dise�os disponibles" :pack '(:side :top :fill :x)
				(radio-button solido :text "Continua" :pack '(:side :top :fill :x) :variable "pa" :value 1)
				(radio-button punto :text "Puntos" :pack '(:side :top :fill :x)  :variable "pa"  :value 2)
				(radio-button raya :text "Rayas" :pack '(:side :top :fill :x)   :variable "pa" :value 3)
				(radio-button raya-punto :text "Rayas y Puntos" :pack '(:side :top :fill :x)  :variable "pa" :value 4)
				(radio-button raya-punto-punto :text "Raya, Punto y Punto" :pack '(:side :top :fill :x)  :variable "pa" :value 5))
		    (button b-ok :text "Ok" :pack '(:side :right :padx 3 :pady 5)
			    :command (lambda ()
				       (ltk::break-mainloop)))
		    (button b-cancel :text "Cancelar" :pack '(:side :right :padx 3 :pady 5)
			    :command (lambda ()
				       (setf ok nil)
				       (ltk::break-mainloop))))
	(dolist (r (list solido punto raya raya-punto raya-punto-punto))
	  (setf (command r) (lambda (val) (declare (ignore val)) (pat r))))
	(mainloop)
	(withdraw top-frame)
	(when ok patron)))))

(defun cambiar-pattern (tab)
  (with-slots (lienzo perfil-activo) tab
    (let ((pattern (ventana-patterns "")))
      (when pattern
	(setf (pattern perfil-activo) pattern)
	(itemconfigure lienzo (linea-perfil perfil-activo) "dash" pattern)
	(when (etiqueta perfil-activo)
	  (itemconfigure lienzo (linea-etiqueta perfil-activo) "dash" pattern))))))

(defun copiar-perfil (tab)
  (with-slots (perfil-activo) tab
    (if (ac perfil-activo)
	(setf *perfil-copiado* (copiar-perfil-objeto perfil-activo))
	(do-msg *mensaje-copiar*))))

(defun pegar-perfil (tab)
  (when *perfil-copiado*
    (with-slots (perfil-ppal perfiles perfil-activo lienzo cursor cursor-x0 eje-x? eje-y? holgura) tab
      (if (ac perfil-ppal)
	  (progn
	    (setf perfil-activo *perfil-copiado*)
	    (agregar-perfil perfil-activo tab))
	  (do-msg *mensaje-pegar*)))))

(defun agregar-perfil (perfil tab)
  (with-slots (perfiles perfil-ppal perfil-activo cursor cursor-x0 lienzo holgura holgura-eje-x holgura-eje-y eje-x? eje-y? dispersion-entry) tab
    (setf (lista-coords perfil) (crear-lista-coords perfil)
	  perfiles (append perfiles (list perfil))
	  (ppal perfil) perfil-ppal
	  perfil-activo perfil)
    (crear-coeficientes-transformacion! perfil lienzo holgura (if (value eje-x?) holgura-eje-x 0) (if (value eje-y?) holgura-eje-y 0))
    (actualizar-perfiles-box tab)
    (when (ac perfil)
      (setf (text dispersion-entry) (format nil "~$" (ac perfil))))
    (itemconfigure lienzo cursor "fill" (color perfil))
    (if cursor-x0
	(itemconfigure lienzo cursor-x0 "fill" (color perfil)))
    (crear-linea-perfil! perfil lienzo)
    (dolist (bandera (banderas perfil))
      (crear-atributos-bandera! bandera lienzo))
    (mostrar-ejes! perfil tab)))

(defun operar-un-perfil (tab funcion sufijo-op &rest parametros)
  (with-slots (perfil-activo perfil-ppal perfiles lienzo cursor cursor-x0 holgura) tab
    (with-slots (1-d longitud nombre) perfil-activo
      (let ((1-d-resultado (make-array longitud)))
	(dotimes (i longitud)
	  (setf (svref 1-d-resultado i) (apply funcion (svref 1-d i) parametros)))
	(let ((perfil (make-instance 'perfil :1-d 1-d-resultado :longitud longitud
				     :minimo (reduce #'min 1-d-resultado) :maximo (reduce #'max 1-d-resultado)
				     :nombre (indexar-nombre sufijo-op nombre perfiles))))
	  (setf (ac perfil) (ac perfil-activo)
		(bc perfil) (bc perfil-activo))
	  (agregar-perfil perfil tab))))))

(defun operar-dos-perfiles (tab funcion nombre-op texto-op sufijo-op)
  (with-slots (perfil-activo perfil-ppal perfiles lienzo cursor cursor-x0 holgura) tab
    (multiple-value-bind (i-1 i-2)
	(ventana-seleccion-2 perfiles nombre-op texto-op)
      (when (and i-1 i-2)
	(let* ((p-1 (nth i-1 perfiles))
	       (p-2 (nth i-2 perfiles))
	       (longitud-nueva (min (longitud p-1) (longitud p-2)))
	       (1-d-resultado (make-array longitud-nueva))
	       (l0 (max (x->l p-1 0) (x->l p-2 0)))
	       (delta-l (min (ac p-1) (ac p-2))))
	  (dotimes (i longitud-nueva)
	    (let ((op-1 (svref (1-d p-1) (round (l->x p-1 (+ l0 (* i delta-l))))))
		  (op-2 (svref (1-d p-2) (round (l->x p-2 (+ l0 (* i delta-l)))))))
	      (setf (svref 1-d-resultado i) (funcall funcion op-1 op-2))))
	  (let ((perfil (make-instance 'perfil :1-d 1-d-resultado :longitud longitud-nueva
				       :minimo (reduce #'min 1-d-resultado) :maximo (reduce #'max 1-d-resultado)
				       :nombre (indexar-nombre sufijo-op (format nil "~a-~a" (nombre p-1) (nombre p-2)) perfiles))))
	    (setf (ac perfil) delta-l
		  (bc perfil) l0)
	    (agregar-perfil perfil tab)))))))

(defun normalizar (tab)
  (operar-un-perfil tab #'normalizar-fun "norm" (minimo (perfil-activo tab)) (- (maximo (perfil-activo tab)) (minimo (perfil-activo tab)))))
(defun normalizar-fun (op-1 minimo delta)
  (float (/ (- op-1 minimo) delta)))

(defun pedir-numero (texto defecto)
  (let ((valor (read-from-string (or (input-box (format nil "N�mero para ~a:" texto)
						:title "Ingreso de datos" :default defecto)
				     "") nil nil)))
    (if (numberp valor) valor)))

(defun sumar-1-perfil (tab)
  (let ((numero (pedir-numero "sumar" "0")))
    (when numero
      (operar-un-perfil tab #'sumar-1-fun "suma" numero))))
(defun sumar-1-fun (op-1 n)
  (+ op-1 n))

(defun multiplicar-1-perfil (tab)
  (let ((numero (pedir-numero "multiplicar" "1")))
    (when numero
      (operar-un-perfil tab #'multiplicar-1-fun "prod" numero))))
(defun multiplicar-1-fun (op-1 n)
  (* op-1 n))

(defun dividir-dos-perfiles (tab)
  (operar-dos-perfiles tab #'dividir-dos-perfiles-fun "Divisi�n" "...dividido por..." "div"))
(defun dividir-dos-perfiles-fun (op-1 op-2)
  (cond ((and (= 0 op-1) (= 0 op-2)) 1)
	((= 0 op-1) 0)
	((= 0 op-2) 1);(error "Division por cero"))
	(t (/ op-1 op-2))))

(defun sumar-dos-perfiles (tab)
  (operar-dos-perfiles tab #'sumar-dos-perfiles-fun "Suma" "...sumado a..." "sum"))
(defun sumar-dos-perfiles-fun (op-1 op-2)
  (+ op-1 op-2))

(defun restar-dos-perfiles (tab)
  (operar-dos-perfiles tab #'restar-dos-perfiles-fun "Diferencia" "...restado a..." "res"))
(defun restar-dos-perfiles-fun (op-1 op-2)
  (- op-1 op-2))

(defun multiplicar-dos-perfiles (tab)
  (operar-dos-perfiles tab #'multiplicar-dos-perfiles-fun "Producto" "...multiplicado por..." "prod"))
(defun multiplicar-dos-perfiles-fun (op-1 op-2)
  (* op-1 op-2))

(defun borrar-perfil (tab)
  (with-slots (perfil-activo perfil-ppal perfiles lienzo cursor dispersion-entry) tab
    (cond ((cdr perfiles)
	   (when (ask-yesno "�Realmente quer�s borrar el perfil?" :title "Ojo que no hay vuelta atr�s...")
	       (borrar-dibujos perfil-activo lienzo)
	       (setf perfiles (remove perfil-activo perfiles)
		     perfil-ppal (car perfiles)
		     perfil-activo perfil-ppal)
	       (dolist (p perfiles)
		 (setf (ppal p) perfil-ppal))
	       (actualizar-perfiles-box tab)
	       (notebook-tab (master tab) tab "text" (nombre perfil-ppal))
	       (mostrar-ejes! perfil-ppal tab)
	       (if (ac perfil-activo)
		   (setf (text dispersion-entry) (format nil "~$" (ac perfil-activo))))
	       (itemconfigure lienzo cursor "fill" (color perfil-activo))))
	  (t  (if (ask-yesno *mensaje-borrar* :title "Ojo que se cierra la pesta�a...")
		  (notebook-forget (master tab) tab))))))

(defun borrar-etiquetas (tab)
  (with-slots (perfil-activo lienzo) tab
    (when (ask-yesno "�Realmente quer�s borrar todas las etiquetas del perfil?"
		     :title "Ojo que no hay vuelta atr�s...")
      (dolist (bandera (banderas perfil-activo))
	(borrar-dibujos-bandera lienzo bandera))
      (setf (banderas perfil-activo) nil))))

(defun indexar-nombre (sufijo nombre perfiles)
  (do ((nombre-nuevo (format nil "~a-~a" nombre sufijo) (format nil "~a-~a-~a" nombre sufijo i))
       (i 1 (+ i 1)))
      ((not (find nombre-nuevo perfiles :key #'nombre :test #'string-equal)) nombre-nuevo)))

(defun auto-calibracion (tab)
  (with-slots (perfil-activo perfil-ppal lienzo dispersion-entry) tab
    (if (ppal? perfil-activo)
	(if (> (length (banderas perfil-activo)) 1)
	    (progn
	      (dolist (b (banderas perfil-activo))
		(itemconfigure lienzo (etiqueta b) "text" (baricentro-x b)))
	      (multiple-value-bind (lx ll) (ventana-calibracion 2 (mapcar #'baricentro-x
									  (subseq (banderas perfil-activo) 0 2)))
		(when lx
		  (crear-coeficientes-calibracion! perfil-activo (first lx) (second lx)
						   (first ll) (second ll))
		  (setf (text dispersion-entry) (format nil "~$" (ac perfil-activo)))))
	      (when (ac perfil-activo)
		(dolist (b (banderas perfil-activo))
		  (itemconfigure lienzo (etiqueta b) "text" (x->l perfil-activo (baricentro-x b))))))
	    (do-msg *mensaje-calibracion-2*))
	(do-msg *mensaje-calibracion*))))

(defun cambiar-nombre (tab)
  (with-slots (perfil-ppal perfil-activo) tab
    (let ((nombre (input-box "Nuevo nombre para el perfil:"
			     :title "Ingreso de datos" :default (nombre perfil-activo))))
      (when nombre
	(setf (nombre perfil-activo) nombre)
	(actualizar-perfiles-box tab)
	(if (ppal? perfil-activo)
	    (notebook-tab (master tab) tab "text" nombre))))))

(defun asegurar-extension (nombre extension)
  (if (string-equal (pathname-type nombre) extension)
      nombre
      (format nil "~a.~a" (pathname-name nombre) extension)))

(defun grabar-ventana (tab)
  (with-slots (perfiles nombre-archivo) tab
    (if nombre-archivo
	(grabar-perfiles perfiles nombre-archivo)
	(grabar-ventana-como tab))))

(defun grabar-ventana-como (tab)
  (with-slots (perfiles nombre-archivo) tab
    (let ((nombre (get-save-file :filetypes '(("Perfiles" "*.annie")))))
      (unless (string= "" nombre)
	(grabar-perfiles perfiles
			 (setf nombre-archivo (asegurar-extension nombre "annie")))))))

(defun exportar-postscript (tab)
    (with-slots (lienzo) tab
      (let ((nombre-archivo (get-save-file :filetypes '(("Archivos postscript" "*.ps")))))
	(unless (string= "" nombre-archivo)
	  (multiple-value-bind (limites) (bbox lienzo)
	    (postscript lienzo nombre-archivo :pagewidth (- (nth 2 limites) (nth 0 limites) -20)
			:pageheight (- (nth 3 limites) (nth 1 limites) -20)))))))

(defun ventana-pdf (lienzo)
  (let* ((*exit-mainloop* nil)
	 (ok t)
	 bordes)
    (labels ((bordes? (radio)
	       (setf bordes
		     (case (value radio)
		       (1 nil)
		       (2 (let* ((x0 (xsc->xc lienzo 0))           (y0 (ysc->yc lienzo 0))
				 (x1 (+ x0 (window-width lienzo))) (y1 (+ y0 (window-height lienzo))))
			    (format nil "-bbox {~a ~a ~a ~a}" x0 y0 x1 y1)))))))
      (with-widgets
	  (toplevel top-frame :title "Opciones de PDF"
		    (labelframe marco :text "L�mites de gr�fico" :pack '(:side :top :fill :x)
				(radio-button todo :text "Toda la pesta�a" :pack '(:side :top :fill :x) :variable "pa" :value 1)
				(radio-button visible :text "S�lo la parte visible" :pack '(:side :top :fill :x)  :variable "pa"  :value 2))
		    (button b-ok :text "Ok" :pack '(:side :right :padx 3 :pady 5)
			    :command (lambda ()
				       (ltk::break-mainloop)))
		    (button b-cancel :text "Cancelar" :pack '(:side :right :padx 3 :pady 5)
			    :command (lambda ()
				       (setf ok nil)
				       (ltk::break-mainloop))))
	(dolist (r (list todo visible))
	  (setf (command r) (lambda (val) (declare (ignore val)) (bordes? r))))
	(setf (value todo) 1)
	(mainloop)
	(withdraw top-frame)
	(when ok bordes)))))

(defun exportar-pdf (tab)
  (with-slots (lienzo) tab
    (let ((opciones (ventana-pdf lienzo))
	  (nombre-archivo (get-save-file :filetypes '(("Archivos PDF" "*.pdf")))))
      (unless (string= "" nombre-archivo)
	(ltk::format-wish "pdf4tcl::new mypdf -paper a4 -margin 15mm -landscape t")
	(ltk::format-wish "mypdf metadata -author ~s -creator \"~a (~a)\"" *autor* *nombre-programa* *version*)
	(ltk::format-wish "mypdf startPage")
	(ltk::format-wish "mypdf setFont 8 Helvetica")
	(ltk::format-wish "mypdf canvas ~a ~@[~a~]" (widget-path lienzo) opciones)
	(ltk::format-wish "mypdf write -file ~s" (asegurar-extension nombre-archivo "pdf"))
	(ltk::format-wish "mypdf destroy")))))

(defun cerrar-pesta�a (tab)
  (when (ask-yesno *mensaje-cerrar* :title "Ojo que se cierra la pesta�a...")
    (notebook-forget (master tab) tab)))

(defun cargar-pdf4tcl ()
    (ltk::format-wish "source pdf4tcl084/stdmetrics.tcl")
    (ltk::format-wish "source pdf4tcl084/glyph2uni.tcl")
    (ltk::format-wish "source snit/snit.tcl")
    (ltk::format-wish "source pdf4tcl084/pdf4tcl.tcl")
    (ltk::format-wish "package require pdf4tcl"))

(defun cargar-tktable ()
  (ltk::format-wish #-(or :win32
			  :mswindows)
		    "load Tktable2.10/libTktable2.10.so"
		    #+(or :win32
			  :mswindows)
		    "load Tktable2.11/Tktable211")
  (ltk::format-wish "package require Tktable"))

(defun annie (&optional nombre-perfil)
  (setf *debug-tk* nil
	*perfil-copiado* nil)
  (let ((ltk:*wish-pathname* (namestring (merge-pathnames #-(or :win32
								:mswindows)
							  "tclkit85-linux-x86_64"
							  #+(or :win32
								:mswindows)
							  "tclkit-win32.upx.exe"))))
    (with-ltk ()
      (cargar-pdf4tcl)
      (cargar-tktable)
      (let* ((marco-perfiles (make-instance 'frame))
	     (mn (make-instance 'notebook :master marco-perfiles))
	     (pesta�as nil)
	     (mb (make-menubar))
	     (mb-archivo (make-menu mb "Archivo"))
	     (mb-edicion (make-menu mb "Edici�n"))
	     (mb-calibracion (make-menu mb "Calibraci�n"))
	     (mb-aritmetica (make-menu mb "Aritm�tica"))
	     (mb-aritmetica-1 (make-menu mb-aritmetica "Para 1 perfil"))
	     (mb-aritmetica-2 (make-menu mb-aritmetica "Para 2 perfiles"))
	     (mb-herramientas (make-menu mb "Herramientas"))
	     (mb-ayuda (make-menu mb "Ayuda")))
	(labels ((crear-pesta�a (funcion &rest parametros)
		   (multiple-value-bind (perfiles nombre-archivo) (apply funcion parametros)
		     (when perfiles
		       (let ((vp (make-instance 'ventana-perfil :perfiles perfiles :nombre-archivo nombre-archivo :master mn)))
			 (push vp pesta�as)
			 (notebook-add mn vp :text (nombre (car perfiles)))
			 (notebook-select mn vp))))))
	       ;;;;; ARCHIVO ;;;;;
	  (let ((m-crear  (make-menubutton mb-archivo "Crear perfil desde imagen FITS..."
					   (lambda () (crear-pesta�a #'abrir-archivo-fits))))
		(m-crear-dat  (make-menubutton mb-archivo "Crear perfil desde archivo DAT..."
					       (lambda () (crear-pesta�a #'abrir-archivo-dat))))
		(m-sep-1 (add-separator mb-archivo))
		(m-abrir (make-menubutton mb-archivo "Abrir pesta�a ANNIE..."
					  (lambda () (crear-pesta�a #'abrir-archivo-annie))))
		(m-grabar (make-menubutton mb-archivo "Grabar pesta�a ANNIE"
					   (lambda () (grabar-ventana (tab-seleccionado mn pesta�as)))))
		(m-grabar-como (make-menubutton mb-archivo "Grabar pesta�a ANNIE como..."
						(lambda () (grabar-ventana-como (tab-seleccionado mn pesta�as)))))
		(m-sep-2 (add-separator mb-archivo))
		;;		(m-post  (make-menubutton mb-archivo "Exportar postscript..."
		;;					  (lambda () (exportar-postscript (tab-seleccionado mn pesta�as)))))
		(m-pdf  (make-menubutton mb-archivo "Exportar pesta�a como PDF..."
					 (lambda () (exportar-pdf (tab-seleccionado mn pesta�as)))))
		(m-sep-3 (add-separator mb-archivo))
		(m-cerrar (make-menubutton mb-archivo "Cerrar pesta�a..."
					   (lambda () (cerrar-pesta�a (tab-seleccionado mn pesta�as)))))
		(m-salir (make-menubutton mb-archivo "Salir"
					  (lambda () (when (ask-yesno *mensaje-salir* :title "Ojo que nos vamos...")
						       (setf *exit-mainloop* t)))))
               ;;;;; EDICI�N ;;;;;
		(m-copiar (make-menubutton mb-edicion "Copiar perfil"
					   (lambda () (copiar-perfil (tab-seleccionado mn pesta�as)))))
		(m-pegar  (make-menubutton mb-edicion "Pegar perfil"
					   (lambda () (pegar-perfil (tab-seleccionado mn pesta�as)))))
		(m-sep-4 (add-separator mb-edicion))
		(m-nombre (make-menubutton mb-edicion "Cambiar nombre..."
					   (lambda () (cambiar-nombre (tab-seleccionado mn pesta�as)))))
		(m-color (make-menubutton mb-edicion "Cambiar color..."
					  (lambda () (cambiar-color (tab-seleccionado mn pesta�as)))))
		(m-pattern (make-menubutton mb-edicion "Cambiar dise�o de l�nea..."
					    (lambda () (cambiar-pattern (tab-seleccionado mn pesta�as)))))
		(m-sep-5 (add-separator mb-edicion))
		(m-borrar (make-menubutton mb-edicion "Borrar perfil..."
					   (lambda () (borrar-perfil (tab-seleccionado mn pesta�as)))))
		(m-borrar-etiquetas (make-menubutton mb-edicion "Borrar etiquetas..."
						     (lambda () (borrar-etiquetas (tab-seleccionado mn pesta�as)))))
               ;;;;; CALIBRACI�N ;;;;;
		(m-auto-cal (make-menubutton mb-calibracion "Auto-calibraci�n..."
					     (lambda () (auto-calibracion (tab-seleccionado mn pesta�as)))))
	       ;;;;; ARITM�TICA 1 ;;;;;
		(m-normalizacion (make-menubutton mb-aritmetica-1 "Normalizaci�n"
						  (lambda () (normalizar (tab-seleccionado mn pesta�as)))))
		(m-sep-6 (add-separator mb-aritmetica-1))
		(m-suma-1 (make-menubutton mb-aritmetica-1 "Sumar..."
					   (lambda () (sumar-1-perfil (tab-seleccionado mn pesta�as)))))
		(m-multiplicar-1 (make-menubutton mb-aritmetica-1 "Multiplicar..."
					   (lambda () (multiplicar-1-perfil (tab-seleccionado mn pesta�as)))))
	       ;;;;; ARITM�TICA 2 ;;;;;
		(m-suma (make-menubutton mb-aritmetica-2 "Sumar..."
					 (lambda () (sumar-dos-perfiles (tab-seleccionado mn pesta�as)))))
		(m-resta (make-menubutton mb-aritmetica-2 "Restar..."
					  (lambda () (restar-dos-perfiles (tab-seleccionado mn pesta�as)))))
		(m-producto (make-menubutton mb-aritmetica-2 "Multiplicar..."
					     (lambda () (multiplicar-dos-perfiles (tab-seleccionado mn pesta�as)))))
		(m-division (make-menubutton mb-aritmetica-2 "Dividir..."
					     (lambda () (dividir-dos-perfiles (tab-seleccionado mn pesta�as)))))
		;;;;; HERRAMIENTAS ;;;;;
		(m-elementos (make-menubutton mb-herramientas "Elementos..."
					      (lambda ()
						(setf *ventana-elementos* (crear-ventana-elementos)))))
               ;;;;; AYUDA ;;;;;
		(m-creditos (make-menubutton mb-ayuda "Cr�ditos" (lambda () (do-msg *creditos* :title "Cr�ditos")))))
	    (declare (ignore m-sep-1 m-sep-2 m-sep-3 m-sep-4 m-sep-5 m-sep-6 m-crear m-crear-dat m-abrir m-salir m-creditos m-elementos))
	    (wm-title *tk* (format nil "~a - v~a (~a)" *nombre-programa* *version* *a�o*))
	    (labels ((habilitar-menues ()
		       (loop for i in (list m-grabar m-grabar-como m-pdf m-cerrar
					    m-copiar m-pegar m-nombre m-color m-pattern m-borrar m-borrar-etiquetas
					    m-normalizacion m-suma-1 m-multiplicar-1
					    m-suma m-resta m-producto m-division
					    m-auto-cal)
			  do (configure i "state" (if (string-equal "" (notebook-tabs mn)) "disabled" "normal")))
		       (unless (string-equal "" (notebook-tabs mn))
			 (loop for i in (list m-copiar m-pegar
					      m-normalizacion m-suma-1 m-multiplicar-1
					      m-suma m-resta m-producto m-division)
			    do (configure i "state" (if (ac (perfil-activo (tab-seleccionado mn pesta�as))) "normal" "disabled"))))))
	      (pack marco-perfiles :fill :both :expand 1)
	      (pack mn :fill :both :expand 1)
	      (set-geometry *tk* 1000 550 0 0)
	      (bind mn "<<NotebookTabChanged>>"
		    (lambda (event) (declare (ignore event)) (habilitar-menues)))
	      (when nombre-perfil
		(crear-pesta�a #'leer-archivo-annie nombre-perfil))
	      (habilitar-menues))))))))

(defun grabar-perfiles (perfiles nombre-archivo)
  (with-open-file (s nombre-archivo :direction :output :if-exists :supersede)
    (dolist (perfil perfiles)
      (format s ":BANDERAS~%")
      (dolist (bandera (banderas perfil))
	(format s "(:baricentro-x ~a :pos-x0 ~a :pos-y0 ~a :pos-x1 ~a :pos-y1 ~a :texto ~s)~%"
		(baricentro-x bandera) (pos-x0 bandera) (pos-y0 bandera) (pos-x1 bandera) (pos-y1 bandera)
		(texto bandera)))
      (format s ":PERFIL~%")
      (format s "(:nombre ~s :color ~s :pattern ~s :ac ~s :bc ~s :ax ~s :bx ~s :ai ~s :bi ~s :minimo ~a :maximo ~a :zx ~a :zy ~a :desp-y ~a :longitud ~a~%:1-d ~s)~%"
	      (nombre perfil) (color perfil) (pattern perfil) (ac perfil) (bc perfil) (ax perfil) (bx perfil) (ai perfil) (bi perfil)
	      (minimo perfil) (maximo perfil) (zx perfil) (zy perfil) (desp-y perfil) (longitud perfil) (1-d perfil)))))

(defun abrir-archivo-annie ()
  (let* ((nombre-archivo (get-open-file :filetypes '(("Perfiles" "*.annie")) :initialdir *directorio-inicio* :title "Abrir perfil")))
    (unless (string= nombre-archivo "")
      (setf *directorio-inicio* (pathname-directory (pathname nombre-archivo)))
      (leer-archivo-annie nombre-archivo))))

(defun leer-archivo-annie (nombre-archivo)
  (let (tag banderas perfiles)
    (with-open-file (in nombre-archivo)
      (do ((linea (read in nil) (read in nil)))
	  ((not linea))
	(if (symbolp linea)
	    (setf tag linea)
	    (case tag
	      (:BANDERAS (push (apply 'make-instance 'bandera linea) banderas))
	      (:PERFIL   (push (apply #'make-instance 'perfil :banderas banderas linea) perfiles)
			 (dolist (b banderas)
			   (setf (perfil b) (car perfiles)))
			 (setf banderas nil))))))
    (setf perfiles (nreverse perfiles))
    (dolist (perfil perfiles)
      (setf (ppal perfil) (car perfiles)
	    (lista-coords perfil) (crear-lista-coords perfil)))
    (values perfiles nombre-archivo)))

(defun crear-lista-coords (perfil)
  (make-list (* 2 (longitud perfil)) :initial-element 0d0))

(defun abrir-archivo-fits ()
  (let ((metodo 'promedio)) ;(ventana-metodo-1d)))
    (when metodo
      (let* ((archivo (get-open-file :filetypes '(("Im�genes FITS" "*.fit *.fits")) :initialdir *directorio-inicio* :title "Abrir imagen FITS"))
	     (imagen (leer-fits archivo)))
	(when imagen
	  (setf *directorio-inicio* (pathname-directory (pathname archivo)))
	  (list (crear-perfil imagen (pathname-name (pathname archivo)) metodo)))))))

(defun abrir-archivo-dat ()
  (let* ((nombre-archivo (get-open-file :filetypes '(("Perfiles .DAT" "*.dat")) :initialdir "J_PASP_110_863/" :title "Abrir perfil .dat")))
    (unless (string= nombre-archivo "")
      (leer-archivo-dat nombre-archivo))))

(defun leer-archivo-dat (nombre-archivo)
  (with-open-file (in nombre-archivo)
    (let (perfil 1-d l-0 l-1)
      (do ((linea (read-line in nil) (read-line in nil)))
	  ((not linea))
	(let ((l (read-from-string (subseq linea 0 7))))
	  (unless l-0 (setf l-0 l))
	  (setf l-1 l)
	  (push (read-from-string (subseq linea 8 17)) 1-d)))
      (setf 1-d (reverse (coerce 1-d 'vector)))
      (setf perfil (make-instance 'perfil :1-d 1-d :longitud (length 1-d) :minimo (reduce #'min 1-d) :maximo (reduce #'max 1-d)
				  :nombre (pathname-name (pathname nombre-archivo)))
	    (lista-coords perfil) (crear-lista-coords perfil)
	    (ppal perfil) perfil)
      (crear-coeficientes-calibracion! perfil 0 (1- (longitud perfil)) l-0 l-1)
      (list perfil))))

(defun promedio (vector longitud)
  (/ (reduce #'+ vector) longitud))

(defun calcular-valor-1d (columna alto metodo)
  (case metodo
    (promedio    (promedio columna alto))
    (mediana-10  (promedio (subseq (sort columna #'<) (round (* alto 1/10)) (round (* alto 9/10))) alto))
    (mediana-30  (promedio (subseq (sort columna #'<) (round (* alto 3/10)) (round (* alto 7/10))) alto))))

(defun crear-perfil (imagen-fits nombre metodo)
  (let* ((data   (data imagen-fits))
	 (ancho  (array-dimension data 0))
	 (alto   (array-dimension data 1))
	 (1-d (make-array ancho))
	 (columna (make-array alto))
	 perfil)
    (dotimes (i ancho)
      (dotimes (j alto)
	(setf (svref columna j) (float (aref data i j) 1d0)))
      (setf (svref 1-d i) (float (calcular-valor-1d columna alto metodo) 1.0)))
    (setf perfil (make-instance 'perfil :1-d 1-d :longitud (length 1-d) :minimo (reduce #'min 1-d) :maximo (reduce #'max 1-d)
				:nombre nombre)
	  (lista-coords perfil) (crear-lista-coords perfil)
	  (ppal perfil) perfil)
    perfil))

(defun ventana-metodo-1d ()
  (let* ((*exit-mainloop* nil)
	 (ok t)
	 (metodo 'promedio))
    (labels ((metodo? (radio)
	       (setf metodo
		     (case (value radio)
		       (1 'promedio)
		       (2 'mediana-10)
		       (3 'mediana-30)))))
      (with-widgets
	  (toplevel top-frame :title "M�todo para el c�lculo del perfil"
		    (labelframe marco :text "" :pack '(:side :top :fill :x)
				(radio-button promedio :text "Promedio" :pack '(:side :top :fill :x) :variable "pa" :value 1)
				(radio-button mediana-10 :text "Mediana (10%)" :pack '(:side :top :fill :x)  :variable "pa"  :value 2)
				(radio-button mediana-30 :text "Mediana (30%)" :pack '(:side :top :fill :x)  :variable "pa"  :value 3))
		    (button b-ok :text "Ok" :pack '(:side :right :padx 3 :pady 5)
			    :command (lambda ()
				       (ltk::break-mainloop)))
		    (button b-cancel :text "Cancelar" :pack '(:side :right :padx 3 :pady 5)
			    :command (lambda ()
				       (setf ok nil)
				       (ltk::break-mainloop))))
	(dolist (r (list promedio mediana-10 mediana-30))
	  (setf (command r) (lambda (val) (declare (ignore val)) (metodo? r))))
	(setf (value promedio) 1)
	(mainloop)
	(withdraw top-frame)
	(when ok metodo)))))

(defun ventana-seleccion-2 (perfiles titulo operacion)
  (let ((*exit-mainloop* nil)
	(ok t))
      (with-widgets
	  (toplevel top-frame :title titulo
		    (frame marco :pack '(:side :top :fill :x)
			   (labelframe marco-1 :text "Perfil 1" :pack '(:side :left :fill :x)
				       (listbox lista-1 :pack '(:side :top :fill :x :padx 5 :pady 5) :exportselection "false"))
			   (labelframe marco-2 :text "Perfil 2" :pack '(:side :right :fill :x)
				       (listbox lista-2 :pack '(:side :top :fill :x :padx 5 :pady 5) :exportselection "false"))
			   (label op :text operacion :pack '(:side :right :fill :x :padx 10)))
		    (button b-ok :text "Ok" :pack '(:side :right :padx 3 :pady 5)
			    :command (lambda ()
				       (ltk::break-mainloop)))
		    (button b-cancel :text "Cancelar" :pack '(:side :right :padx 3 :pady 5)
			    :command (lambda ()
				       (setf ok nil)
				       (ltk::break-mainloop))))
	(listbox-append lista-1 (mapcar #'nombre perfiles))
	(listbox-append lista-2 (mapcar #'nombre perfiles))
	(mainloop)
	(withdraw top-frame)
	(when ok (values (car (listbox-get-selection lista-1))
			 (car (listbox-get-selection lista-2)))))))

(defun crear-ventana-elementos ()
  (setf *catalogo-elementos* (leer-catalogo))
  (let ((catalogo-elementos (copy-tree *catalogo-elementos*)))
    (with-widgets
	(toplevel top-frame :title "Elementos"
		  (frame marco :pack '(:side :top :fill :both :expand t)
			 (scrolled-table tabla :titlerows 1 :titlecols 0 :data catalogo-elementos :pack '(:side :left :fill :both :expand t))
			 (labelframe marco-elementos :text "Elementos" :pack '(:side :left :fill :both :expand t :padx 10)
				     (scrolled-listbox tabla-elementos :pack '(:side :top :fill :y :expand t))
				     (frame marquito :pack '(:side :top :ipadx 3 :ipady 3)
					    (button b-todos :text "Todos" :pack '(:side :right :padx 3 :pady 0))
					    (button b-ninguno :text "Ninguno" :pack '(:side :left :padx 3 :pady 0)))
				     (check-button filtro-auto :text "Filtrar autom�ticamente" :pack '(:side :top :fill :x :padx 3))
				     (button b-filtrar :text "<- Filtrar" :pack '(:side :bottom :padx 3 :pady 5 :fill :x)
					     :command (lambda () (filtrar-tabla-elementos (table tabla) catalogo-elementos tabla-elementos))))))

      (bind (listbox tabla-elementos) "<<ListboxSelect>>"
	    (lambda (event) (declare (ignore event))
		    (when (value filtro-auto)
		      (filtrar-tabla-elementos (table tabla) catalogo-elementos tabla-elementos))))
      (setf (command filtro-auto) (lambda (valor)
				    (configure b-filtrar :state (if (= 0 valor) "normal" "disabled"))))
      (setf (command b-todos) (lambda ()
				(listbox-select tabla-elementos (list 0 100))
				(when (value filtro-auto)
				  (filtrar-tabla-elementos (table tabla) catalogo-elementos tabla-elementos))))
      (setf (command b-ninguno) (lambda ()
				  (listbox-select tabla-elementos nil)
				  (when (value filtro-auto)
				    (filtrar-tabla-elementos (table tabla) catalogo-elementos tabla-elementos))))
      (configure (listbox tabla-elementos) :selectmode "extended" :exportselection "false")
      (dolist (e (leer-elementos-quimicos))
	(listbox-append tabla-elementos e))
      (listbox-select tabla-elementos (list 0 100))
      (configure (table tabla) "state" "disabled")
      (on-close top-frame (lambda ()
			    (setf *ventana-elementos* nil)
			    (destroy top-frame)))
      (table tabla))))

(defun filtrar-tabla-elementos (tabla catalogo-elementos tabla-elementos)
  (configure tabla "state" "normal")
  (ltk:format-wish "~a delete cols -holddimensions 0 3" (widget-path tabla))
  (setf *catalogo-elementos* (filtrar-por-z catalogo-elementos
					    (listbox-get-selection tabla-elementos)))
  (escribir-tabla tabla *catalogo-elementos*)
  (configure tabla "state" "disabled"))

(defun escribir-tabla (tabla datos)
  (setf (tktable::rows tabla) (length datos))
  (let ((i 0))
    (dolist (fila datos)
      (set-row tabla i fila)
      (incf i))))

(defun leer-elementos-quimicos ()
  (with-open-file (in #-(or :win32
			    :mswindows)
		      "elementos-quimicos-lx.csv"
		      #+(or :win32
			    :mswindows)
		      "elementos-quimicos-ws.csv")
    (do ((elementos nil)
	 (linea (read-line in nil) (read-line in nil)))
	((not linea) (nreverse elementos))
      (push (format nil "~a  (~a)" (string-trim " " (subseq linea 11 24)) (string-trim " " (subseq linea 0 11))) elementos))))

(defun leer-catalogo ()
  (with-open-file (in "VI_71A/illss.dat")
    (do ((catalogo (list (list 'lambda 'elemento 'z)))
	 (linea (read-line in nil) (read-line in nil)))
	((not linea) (nreverse catalogo))
      (let ((lamda (read-from-string (subseq linea 0 11))))
	(when (<= 3900 lamda 7800)
	  (let ((elemento (subseq linea 12 21))
		(z (read-from-string (subseq linea 21 23))))
	    (push (list lamda elemento z) catalogo)))))))

(defun filtrar-valores (catalogo l-central l-delta)
  (remove-if-not #'(lambda (l) (<= (abs (- l l-central)) l-delta)) catalogo :key #'car :start 1))

(defun filtrar-por-z (catalogo lista-z)
  (remove-if-not #'(lambda (z) (find (1- z) lista-z)) catalogo :key #'third :start 1))

(defun filtrar-filas (catalogo l-central l-delta)
  (let ((i-1 (position-if #'(lambda (l) (<= (abs (- l l-central)) l-delta)) catalogo :key #'car :start 1)))
    (values i-1 (when i-1
		  (+ i-1 (count-if #'(lambda (l) (<= (abs (- l l-central)) l-delta)) catalogo :key #'car :start i-1) -1)))))

;;;;;;;;;; Sugerencias de Diego
;;
;; * Que al grabar, se agregue la extensi�n por defecto.---------- (LISTO)
;; * Que al normalizar, de la opci�n de borrar el perfil original.
;; * Opci�n de crear el resultado de una operaci�n en un nueva pesta�a
;; * Posibilidad de agregar la etiqueta en la pesta�a.  ---------- (LISTO)
;; * No mostrar todas las l�neas del cat�logo de l�neas, sino las del espectro visible ------- (LISTO)

;; Historia
;; 1.2.7:   * "memoria" del directorio abierto (s�lo sirve para la apertura de archivos).
;;          * prueba de m�todos para crear perfiles (promedio, mediana, etc.) <- dudoso.
;; 1.2.8:   * saqu� los m�todos citados arriba.
;;          * solucion� un problema con la lectura de archivos FITS de 32 bits de punto flotante.
;; 1.2.9:   * Hacer que las l�neas de las etiquetas sean "quebradas" horizontales y verticales.
;;          * versi�n 0.2 de fits.lisp
;; 1.3.0    * Calcular el baricentro ajust�ndolo como gausiana. <--- se corre (?)

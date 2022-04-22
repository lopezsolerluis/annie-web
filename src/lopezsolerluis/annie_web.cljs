(ns ^:figwheel-hooks lopezsolerluis.annie-web
  (:require
   [goog.dom :as gdom]
   [goog.events :as gevents]
   [reagent.core :as r :refer [atom]]
   [reagent.ratom :as ratom]
   [reagent.dom :as rdom]
   [cljsjs.react-vis :as rvis]
   [clojure.string :as str]
   [lopezsolerluis.traducciones :as trad :refer [app-tr translations]]
   [lopezsolerluis.fits :as fits]
   [lopezsolerluis.espectros-sec-ppal :as espectros :refer [espectros-referencia espectros-referencia-nombres]]
   [lopezsolerluis.metodos-numericos :as mn]
   [lopezsolerluis.save-load-file :as save :refer [write-pestaña read-pestaña write-svg]]))

(set! *print-level* nil)

;; define your app data so that it doesn't get over-written on reload
(defonce pestañas (r/atom {:pestaña-activa nil :pestañas nil}))  ; ¿defonce o def..?
(defonce pestaña-activa (r/cursor pestañas [:pestaña-activa]))
(defonce portapapeles (atom []))
(def etiqueta-activa (atom []))

(def app (gdom/getElement "app"))
(def plot-width (atom nil))
(def plot-height (atom nil))

(def icono-espera (gdom/getElement "loader"))
(def fondo-gris (gdom/getElement "fondogris"))
(def fondo-transparente (gdom/getElement "fondoblanco"))
(def ventana-elementos (gdom/getElement "ventana-elementos"))
(def etiqueta-ok (gdom/getElement "ok-etiqueta"))
(def etiqueta-cancel (gdom/getElement "cancel-etiqueta"))
(def etiqueta-delete (gdom/getElement "delete-etiqueta"))
(def etiqueta-texto (gdom/getElement "etiqueta-texto"))
(def ventana-calibración (gdom/getElement "ventana-calibración"))
(def x1-calibración-number (gdom/getElement "x1-calibración-number"))
(def x2-calibración-number (gdom/getElement "x2-calibración-number"))
(def lambda1-calibración-number (gdom/getElement "lambda1-calibración-number"))
(def lambda2-calibración-number (gdom/getElement "lambda2-calibración-number"))
(def calibración-ok (gdom/getElement "ok-calibración"))
(def calibración-cancel (gdom/getElement "cancel-calibración"))
(def open-fits (gdom/getElement "open-fits"))
(def open-annie (gdom/getElement "open-annie"))
(def menu-principal (gdom/getElement "menuprincipal"))
(def tabs (gdom/getElement "tabs"))
(def ventana-espectros (gdom/getElement "ventana-espectros"))
(def input-espectros (gdom/getElement "input-espectros"))
(def datalist-de-espectros (gdom/getElement "datalist-de-espectros"))
(def espectros-boton-ok (gdom/getElement "ok-espectros"))
(def espectros-boton-cancel (gdom/getElement "cancel-espectros"))
(def copiar-perfil-menu (gdom/getElement "copiar-perfil"))
(def pegar-perfil-menu (gdom/getElement "pegar-perfil"))
(def popup-forms (array-seq (gdom/getElementsByClass "form-popup")))
(def menu-perfil-activo (gdom/getElement "perfil-activo-menu"))
(def perfil-activo-select (gdom/getElement "perfil-activo-select"))
(def boton-zoom-etc (gdom/getElement "boton-zoom-etc"))
(def ventana-zoom-etc (gdom/getElement "ventana-herramientas"))
(def help-window (gdom/getElement "help-window"))
(def credits-window (gdom/getElement "credits-window"))
(def language-selector (gdom/getElement "language"))
(def cambiar-perfil-ventana-menu (gdom/getElement "cambiar-perfil"))
(def ventana-cambiar-perfil (gdom/getElement "ventana-cambiar-perfil"))
(def cambiar-nombre-perfil (gdom/getElement "cambiar-nombre-perfil"))
(def cambiar-color-perfil (gdom/getElement "cambiar-color-perfil"))
(def color-por-defecto-checkbox (gdom/getElement "color-por-defecto"))
(def cambiar-ancho-perfil (gdom/getElement "cambiar-ancho-perfil"))
(def estilos-perfil (array-seq (.getElementsByName js/document "estilo-perfil")))
(def ventana-operar-uno (gdom/getElement "ventana-operar-uno"))
(def operar-uno-input (gdom/getElement "operar-uno-input"))
(def ventana-borrar-perfil (gdom/getElement "ventana-borrar-perfil"))
(def borrar-perfiles-select (gdom/getElement "borrar-perfiles-select"))
(def dispersión-span (gdom/getElement "valor-dispersión"))
(def perfil-activo-operar-uno-título (gdom/getElement "perfil-activo-operar-uno-título"))
(def ventana-operar-dos (gdom/getElement "ventana-operar-dos"))
(def perfil-activo-operar-dos-nombre (gdom/getElement "perfil-activo-operar-dos-nombre"))
(def operación-dos (gdom/getElement "operación-dos"))
(def operar-dos-select (gdom/getElement "operar-dos-select"))
(def conservar-etiquetas-checkbox (gdom/getElement "conservar-etiquetas-checkbox"))
(def alert-window (gdom/getElement "alert-window"))
(def mensaje-alert (gdom/getElement "mensaje-alert"))
(def confirmar-window (gdom/getElement "confirmar-window"))
(def mensaje-confirmar (gdom/getElement "mensaje-confirmar"))
(def ok-confirmar (gdom/getElement "ok-confirmar"))

;; Para que el gráfico pueda hacer "scroll" dentro de un div fijo... casi hacker!
(def alto-header (+ (.-offsetHeight menu-principal) (.-offsetHeight tabs)))
(set! (.. app -style -height)
      (str "calc( 100vh - "  alto-header  "px )"))

(defn get-perfil-activo-nombre []
  (get-in @pestañas [:pestañas @pestaña-activa :perfil-activo]))

(defn get-perfil-activo-key []
  [:pestañas @pestaña-activa :perfiles (get-perfil-activo-nombre)])

(defn get-perfil-activo []
  (get-in @pestañas (get-perfil-activo-key)))

(defn get-pestaña-activa []
  (get-in @pestañas [:pestañas (:pestaña-activa @pestañas)]))

(defn crear-lista-de-espectros []
  (let [clases (sort espectros-referencia-nombres)]
    (doseq [clase clases]
      (let [option (.createElement js/document "option")]
        (set! (.-value option) clase)
        (.appendChild datalist-de-espectros option)))))

(crear-lista-de-espectros)

(defn crear-lista-de-perfiles [select-element con-perfil-activo?]
  (let [pestaña-activa (get-pestaña-activa)
        perfil-activo-nombre (:perfil-activo pestaña-activa)
        perfiles-nombres (remove #{(if-not con-perfil-activo? perfil-activo-nombre)} (keys (:perfiles pestaña-activa)))]
    (gdom/removeChildren select-element)
    (doseq [nombre perfiles-nombres]
      (let [option (.createElement js/document "option")]
        (set! (.-value option) nombre)
        (set! (.-innerHTML option) nombre)
        (.appendChild select-element option)))
    (if con-perfil-activo? (set! (.-value select-element) perfil-activo-nombre))))

(defn encender-espera [on] ; true or false
  (set! (.. icono-espera -style -display) (if on "block" "none"))
  (set! (.. fondo-gris -style -display) (if on "block" "none")))

;; Translation functions
(defn getLanguage []
  (-> (or (.-language js/navigator) (.-userLanguage js/navigator) "en")
      (subs 0 2)))
(def lang (r/atom (keyword (getLanguage))))
(defn traducir
  ([] (traducir @lang))
  ([lang]
   (gdom/setTextContent (gdom/getElement "perfiles-label") (app-tr lang :ventana-zoom-etc/perfil-activo))
   (gdom/setTextContent (gdom/getElement "dispersión") (app-tr lang :dispersión))
   (gdom/setTextContent (gdom/getElement "xpixel") (app-tr lang :xpixel))
   (doseq [key-1 [:menu :ventana-etiqueta :ventana-calibración :ventana-espectros :ventana-zoom-etc
                  :ventana-cambiar-perfil :ventana-operar-uno :ventana-operar-dos :ventana-borrar-perfil
                  :help-window :credits-window]]
     (doseq [key-2 (-> translations :es key-1 keys)]
       (let [el (gdom/getElement (name key-2))]
         (gdom/setTextContent el (app-tr lang (keyword (name key-1) key-2))))))))

(set! (.-value language-selector) (getLanguage))
(defn update-language [evt]
  (reset! lang (.. evt -target -value))
  (traducir))

(traducir)
;; end of translation functions

;; Para encontrar todos los valores asociados a una misma clave (https://stackoverflow.com/questions/28091305/find-value-of-specific-key-in-nested-map/28097404)
(defn find-all-nested
  [m k]
  (->> (tree-seq map? vals m)
       (filter map?)
       (keep k)))

(defn change-ventana
 ([ventana state]  ; state es "block" o "none"
   (change-ventana ventana state fondo-transparente))
 ([ventana state fondo] ; fondo-transparente o fondo-gris (u otro...)
   (set! (.. ventana -style -display) state)
   (set! (.. fondo -style -display) state)))

(defn alert [mensaje]
  (gdom/setTextContent mensaje-alert mensaje)
  (change-ventana alert-window "block" fondo-gris))
  ; (js/alert mensaje))

(defn elegir-nombre [nombres-usados nombre-posible con-numero-siempre?]
   (let [nombres-set (set nombres-usados)]
      (if-not (or con-numero-siempre? (nombres-set nombre-posible))
          nombre-posible
          (loop [n 1]
            (let [nombre ((if con-numero-siempre? keyword identity) (str nombre-posible "-" n))] ; Los pefiles pueden ser strings; las etiquetas las prefierto keywords...
              (if-not (nombres-set nombre)
                      nombre
                      (recur (inc n))))))))

(defn calcular-calibración [x1 x2 lambda1 lambda2]
  (let [a (/ (- lambda2 lambda1) (- x2 x1))
        b (/ (- (* lambda1 x2) (* lambda2 x1)) (- x2 x1))]
     [a b]))

(defn crear-datos-perfil-2d [fits-file]
  (let [data (:data fits-file)]
          ;(apply map + data))) ; Para sumar las columnas (tarda mucho más)
       (map #(reduce + %) data)))  ; Suma sobre las filas, porque el archivo "fits" lo creé 'traspuesto'...¡casi hacker!

(defn normalizar-perfil-2d
  "Converts a vector of values ([x1 x2 x3...]) in the interval [0,1]"
  [perfil-2d]
  (let [{mínimo :min máximo :max} (mn/calcular-extremos perfil-2d)
        [a b] (calcular-calibración mínimo máximo 0 1)] ;el algoritmo para calcular la calibración es el mismo: una función lineal
    (map (fn [n] (+ (* a n) b)) perfil-2d)))

(defn crear-data-para-vis [perfil-2d]
  (mapv (fn [x y] {:x x :y y}) (range) perfil-2d))

(defn calibrado? [perfil]
  (seq (:calibración perfil)))

(defn calcular-dispersión [perfil]
  (if (calibrado? perfil)
      (let [data-vis (:data-vis perfil)
            delta-x (- (:x (second data-vis)) (:x (first data-vis)))
            a (first (:calibración perfil))]
        (* a delta-x))))

(defn actualizar-dispersión-span []
  (gdom/setTextContent dispersión-span (str (or (if-let [dispersión (calcular-dispersión (get-perfil-activo))]
                                                    (.toFixed dispersión 1))
                                                "—"))))

(defn crear-pestaña
  ([nombre-posible data-para-vis] (crear-pestaña nombre-posible data-para-vis []))
  ([nombre-posible data-para-vis calibración]
   (let [nombre (elegir-nombre (keys (:pestañas @pestañas)) nombre-posible false)]
     (swap! pestañas assoc-in [:pestañas nombre] {:perfil-activo nombre})
     (swap! pestañas assoc-in [:pestañas nombre :perfiles nombre]  ; pestaña perfil
                              {:data-vis data-para-vis :color nil :dasharray nil :width 1
                               :calibración calibración :inc-y 0 :fact-y 1 :etiquetas {}})
     (swap! pestañas assoc :pestaña-activa nombre))))

(defn procesar-archivo-fits [fits-file]
  (if (= fits-file :fits-no-simple)
      (js/alert (app-tr @lang :fits-no-valido))
      (let [perfil-2d (crear-datos-perfil-2d fits-file)
            perfil-2d-normalizado (normalizar-perfil-2d perfil-2d)
            data-para-vis (crear-data-para-vis perfil-2d-normalizado)
            nombre (:nombre-archivo fits-file)]
        (crear-pestaña nombre data-para-vis)))
  (encender-espera false))

(defn procesar-pestaña-annie [pestaña-annie-as-string]
  (if-not (map? pestaña-annie-as-string) ; ¿Ser más estricto a la hora de verificar si es una pestaña válida?
          (alert (app-tr @lang :annie-no-válido))
          (let [nombre-posible (first (keys pestaña-annie-as-string))
                nombre (elegir-nombre (keys (:pestañas @pestañas)) nombre-posible false)
                pestaña-original (first (vals pestaña-annie-as-string))
                pestaña (if (= nombre nombre-posible)
                            pestaña-original
                            (clojure.walk/postwalk-replace {nombre-posible nombre} pestaña-original))]
            (swap! pestañas assoc-in [:pestañas nombre] pestaña)
            (swap! pestañas assoc :pestaña-activa nombre)))
  (encender-espera false))

(defn confirmar-operación [texto operación]
  (gdom/setTextContent mensaje-confirmar texto)
  (change-ventana confirmar-window "block" fondo-gris)
  (gevents/removeAll ok-confirmar "click")
  (gevents/listen ok-confirmar "click" (fn [] (operación)
                                              (change-ventana confirmar-window "none" fondo-gris))))

(defn copiar-perfil []
  (let [perfil-activo (get-perfil-activo)]
    (if-not perfil-activo
            (alert (app-tr @lang :no-hay-perfil-que-copiar))
            (let [perfil-activo-nombre (last (get-perfil-activo-key))]
              (if-not (calibrado? perfil-activo)
                      (alert (app-tr @lang :perfil-no-calibrado-no-puede-copiarse))
                      (do
                        (reset! portapapeles [perfil-activo-nombre perfil-activo])
                        (alert (app-tr @lang :perfil-copiado))))))))

(defn agregar-perfil-en-pestaña [nombre perfil]
  (swap! pestañas assoc-in [:pestañas @pestaña-activa :perfiles nombre] (assoc perfil :color nil :dasharray nil)))

(defn pegar-perfil []
  (if (empty? @portapapeles)
      (alert (app-tr @lang :portapapeles-vacío))
      (let [perfil-activo (get-perfil-activo)]
        (if-not (calibrado? perfil-activo)
                (alert (app-tr @lang :perfil-no-calibrado-no-admite-pegado))
                (let [nombre-copiado (first @portapapeles)
                      nombres-en-pestaña (keys (get-in @pestañas (butlast (get-perfil-activo-key))))
                      nombre (elegir-nombre nombres-en-pestaña nombre-copiado false)
                      perfil-pegado (second @portapapeles)]
                  (agregar-perfil-en-pestaña nombre perfil-pegado))))))

(defn agregar-texto-etiqueta []
  (let [perfil-activo (get-perfil-activo)]
    (when (calibrado? perfil-activo)
      (let [texto (str/split-lines (.-value etiqueta-texto))]
        (swap! pestañas assoc-in (conj @etiqueta-activa :texto) texto)))
    (change-ventana ventana-elementos "none")))
(defn cancelar-texto-etiqueta []
  (change-ventana ventana-elementos "none"))
(defn borrar-etiqueta []
  (confirmar-operación (app-tr @lang :confirmar-borrar-etiqueta) #(swap! pestañas update-in (pop @etiqueta-activa) dissoc (last @etiqueta-activa)))
  (change-ventana ventana-elementos "none"))

(defn filtrar-dominio [data x-min x-max]
  (filter (fn [punto] (<= x-min (:x punto) x-max)) data))

(defn calibrar-data-vis [data a b]
  (mapv (fn [{:keys [x y]}] {:x (+ (* a x) b) :y y}) data))
(defn obtener-data [perfil]
  (if (calibrado? perfil)
      (let [[a b] (:calibración perfil)
            inc-y (:inc-y perfil)
            fact-y (:fact-y perfil)]
        (mapv (fn [{:keys [x y]}] {:x (+ (* a x) b) :y (+ (* y fact-y) inc-y)}) (:data-vis perfil)))
      (:data-vis perfil)))
(defn calcular-x-calibrado [perfil x]
  (if (calibrado? perfil)
      (let [[a b] (:calibración perfil)]
        (+ (* a x) b))
      x))
(defn calcular-x-no-calibrado [perfil x]
  (if (calibrado? perfil)
      (let [[a b] (:calibración perfil)]
        (/ (- x b) a))
      x))

(defn modificar-data-vis
  "La coordenada es :x o :y. Si la función lleva parámetros,
  sería bueno crearla con (partial + 10), por ejemplo.
  Aunque quizá sea más seguro algo como #(- % 10)"
  [data-vis coord funcion]
    (map (fn [punto] (update punto coord funcion)) data-vis))

(defn desplazar-perfil-y [perfil-key inc]
  (let [data-vis-key (conj perfil-key :data-vis)
        data-vis (get-in @pestañas data-vis-key)
        data-vis-nuevo (modificar-data-vis data-vis :y (partial + inc))]
    (swap! pestañas assoc-in data-vis-key data-vis-nuevo)))

(defn subir-perfil-activo []
  (swap! pestañas update-in (conj (get-perfil-activo-key) :inc-y) #(+ % 0.05)))
(defn bajar-perfil-activo []
  (swap! pestañas update-in (conj (get-perfil-activo-key) :inc-y) #(- % 0.05)))
(defn reset-y-perfil-activo []
  (swap! pestañas assoc-in (conj (get-perfil-activo-key) :inc-y) 0))

(defn expandir-y-perfil-activo []
  (swap! pestañas update-in (conj (get-perfil-activo-key) :fact-y) #(* % 1.05)))
(defn comprimir-y-perfil-activo []
  (swap! pestañas update-in (conj (get-perfil-activo-key) :fact-y) #(* % 0.95)))
(defn reset-fact-y-perfil-activo []
  (swap! pestañas assoc-in (conj (get-perfil-activo-key) :fact-y) 1))

(defn abrir-ventana-calibración []
  (let [perfil-activo (get-perfil-activo)]
    (if-not perfil-activo
        (alert (app-tr @lang :no-hay-perfil-para-calibrar))
        (let [ultimas-etiquetas (take-last 2 (:etiquetas perfil-activo))]
          (if-not (= 2 (count ultimas-etiquetas))
                  (alert (app-tr @lang :debería-seleccionar-dos-líneas))
                  (let [baricentros (map :x (vals ultimas-etiquetas))
                        x1 (apply min baricentros)
                        x2 (apply max baricentros)]
                    (change-ventana ventana-calibración "block")
                    (.select lambda1-calibración-number)
                    (set! (.-value x1-calibración-number) (.toFixed x1 2))
                    (set! (.-value x2-calibración-number) (.toFixed x2 2))))))))

(defn calibrar-ok []
  (let [lambda1 (js/parseFloat (.-value lambda1-calibración-number))
        lambda2 (js/parseFloat (.-value lambda2-calibración-number))]
       (if (or (js/isNaN lambda1) (js/isNaN lambda2))
           (alert (app-tr @lang :deben-ingresarse-dos-lambdas))
           (let [x1 (js/parseFloat (.-value x1-calibración-number)) ; verificar que son válidos (?)
                 x2 (js/parseFloat (.-value x2-calibración-number))
                 params (calcular-calibración x1 x2 lambda1 lambda2)]
             (swap! pestañas assoc-in (conj (get-perfil-activo-key) :calibración) params)
             (change-ventana ventana-calibración "none")))))
(defn calibrar-cancel []
  (change-ventana ventana-calibración "none"))

(defn abrir-ventana-espectros-dat []
  (change-ventana ventana-espectros "block")
  (.focus input-espectros))
(defn espectros-ok []
  (let [clase (.-value input-espectros)]
     (if-not (espectros-referencia-nombres clase)
        (alert (app-tr @lang :la-clase-es-desconocida))
        (do
          (crear-pestaña clase ((keyword clase) espectros-referencia) [1 0])
          (change-ventana ventana-espectros "none")))))
(defn espectros-cancel []
  (change-ventana ventana-espectros "none"))

(defn abrir-archivo [this tipo]
  (when-not (= "" (-> this .-target .-value))
    (let [^js/File file (-> this .-target .-files (aget 0))]
      (encender-espera true)
      (case tipo
        :fits (fits/read-fits-file file procesar-archivo-fits)
        :annie (read-pestaña file procesar-pestaña-annie))))
  (set! (-> this .-target .-value) ""))

(defn grabar-pestaña-annie []
  (if-not @pestaña-activa
    (alert (app-tr @lang :no-pestaña-activa-para-grabar))
    (write-pestaña @pestaña-activa (get-pestaña-activa))))

(defn grabar-pestaña-svg []
  (if-not @pestaña-activa
    (alert (app-tr @lang :no-pestaña-activa-para-grabar))
    (write-svg @pestaña-activa)))

(defn change-ventana-zoom-etc []
  (if (= (.. ventana-zoom-etc -style -display) "block")
      (do
        (set! (.. ventana-zoom-etc -style -display) "none")
        (set! (.. boton-zoom-etc -style -borderStyle) "outset"))
      (do
        (set! (.. ventana-zoom-etc -style -display) "block")
        (set! (.. boton-zoom-etc -style -borderStyle) "inset"))))

(defn abrir-ventana-cambiar-perfil []
  (if-not @pestaña-activa
    (alert (app-tr @lang :no-hay-perfil-que-modificar))
    (set! (.. ventana-cambiar-perfil -style -display) "block")))

(defn actualizar-ventana-cambiar-perfil []
  (let [perfil-activo (get-perfil-activo)]
    (set! (.-value cambiar-nombre-perfil) (get-perfil-activo-nombre))
    (set! (.-value cambiar-color-perfil) (or (:color perfil-activo) "#000"))
    (set! (.-value cambiar-ancho-perfil) (or (:width perfil-activo) "1"))
    (set! (.-checked color-por-defecto-checkbox) (not (:color perfil-activo)))
    (set! (.-checked (gdom/getElement (or (:dasharray perfil-activo) "solid"))) true)))

(defn cerrar-ventana-cambiar-perfil []
    (set! (.. ventana-cambiar-perfil -style -display) "none"))

(defn cambiar-color-perfil-fn []
  (swap! pestañas assoc-in (conj (get-perfil-activo-key) :color) (if-not (.-checked color-por-defecto-checkbox)
                                                                         (.-value cambiar-color-perfil))))

(defn cambiar-ancho-perfil-fn []
  (let [valor (js/parseInt (.-value cambiar-ancho-perfil))]
    (if-not (js/isNaN valor)
      (swap! pestañas assoc-in (conj (get-perfil-activo-key) :width) valor))))

(defn cambiar-nombre-perfil-fn []
  (let [nombre (.-value cambiar-nombre-perfil)
        nombre-viejo (get-perfil-activo-nombre)]
    (if-not (= -1 (.indexOf (keys (:perfiles (get-pestaña-activa))) nombre))
            (alert (app-tr @lang :el-nombre-pertenece-a-un-perfil-de-la-pestaña))
            (do (swap! pestañas assoc-in [:pestañas @pestaña-activa :perfil-activo] nombre)
                (swap! pestañas update-in [:pestañas @pestaña-activa :perfiles] clojure.set/rename-keys {nombre-viejo nombre})))))

(defn cambiar-estilo-perfil-fn []
  (let [estilo (.-value (.querySelector js/document "input[name=\"estilo-perfil\"]:checked"))]
    (swap! pestañas assoc-in (conj (get-perfil-activo-key) :dasharray) (if-not (= estilo "nil") estilo))))

(defn cambiar-perfil-activo [nombre]
  (swap! pestañas assoc-in [:pestañas @pestaña-activa :perfil-activo] nombre)
  (actualizar-dispersión-span)
  (actualizar-ventana-cambiar-perfil))

(defn do-optizoom []
   (reset! plot-height 10) ; No estoy seguro de si esto es necesario;
   (reset! plot-width 10)  ; ni siquiera de si es útil
   (reset! plot-width nil); (.-offsetWidth app))
   (reset! plot-height nil));(.-offsetHeight app)))

(def mouse (atom {:isDown false :offset {:x 0 :y 0}}))

;; Aritmética de 1 perfil
(defn normalizar-data-vis [data-vis]
  (let [nuevos-y (normalizar-perfil-2d (map :y data-vis))]
    (mapv (fn [x y] {:x x :y y}) (map :x data-vis) nuevos-y)))

(defn normalizar-perfil-activo []
  (let [perfil-activo (get-perfil-activo)
        data-vis-nuevo (normalizar-data-vis (:data-vis perfil-activo))
        nombre-actual (str (get-perfil-activo-nombre) (app-tr @lang :normalizado))
        nombres-en-pestaña (keys (get-in @pestañas (butlast (get-perfil-activo-key))))
        nombre (elegir-nombre nombres-en-pestaña nombre-actual false)]
      (agregar-perfil-en-pestaña nombre (assoc perfil-activo :data-vis data-vis-nuevo))))

(defn abrir-ventana-operar-uno [operación]
  (if-not @pestaña-activa
    (alert (app-tr @lang :no-hay-perfil-que-modificar))
    (do (change-ventana ventana-operar-uno "block")
        (cond (= operación :sumar-uno) (gdom/setTextContent perfil-activo-operar-uno-título (app-tr @lang :sumar-uno-título))
              (= operación :multiplicar-uno) (gdom/setTextContent perfil-activo-operar-uno-título (app-tr @lang :multiplicar-uno-título))
              :else nil)
        (.focus operar-uno-input))))

(defn operar-uno-data-vis [data-vis función numero]
  (let [nuevos-y (map (fn [punto] (función numero (:y punto))) data-vis)]
    (mapv (fn [x y] {:x x :y y}) (map :x data-vis) nuevos-y)))

(defn operar-escalar-perfil-activo [función tag]
  (let [numero (js/parseFloat (.-value operar-uno-input))]
    (if (js/isNaN numero)
        (alert (app-tr @lang :debe-ingresarse-un-número))
        (let [perfil-activo (get-perfil-activo)
              data-vis-nuevo (operar-uno-data-vis (:data-vis perfil-activo) función numero)
              etiquetas (:etiquetas perfil-activo)
              etiquetas-nuevas (into {} (map (fn [[k v]] [k (update v :y (fn [y] (función numero y)))]) etiquetas))
              nombre-actual (str (get-perfil-activo-nombre) (app-tr @lang tag) "(" numero ")")
              nombres-en-pestaña (keys (get-in @pestañas [:pestañas @pestaña-activa :perfiles]))
              nombre (elegir-nombre nombres-en-pestaña nombre-actual false)]
          (agregar-perfil-en-pestaña nombre (assoc perfil-activo
                                                   :data-vis data-vis-nuevo
                                                   :etiquetas (if (.-checked conservar-etiquetas-checkbox) etiquetas-nuevas)))
          (change-ventana ventana-operar-uno "none")))))

(defn operar-uno-perfil-activo []
  (let [título-operación (gdom/getTextContent perfil-activo-operar-uno-título)]
    (cond (= título-operación (app-tr @lang :sumar-uno-título)) (operar-escalar-perfil-activo + :suma-escalar)
          (= título-operación (app-tr @lang :multiplicar-uno-título)) (operar-escalar-perfil-activo * :producto-escalar))))

;; Aritmética de dos perfiles
(defn abrir-ventana-operar-dos [operación]
  (if-not @pestaña-activa
    (alert (app-tr @lang :no-hay-perfil-que-modificar))
    (let [perfiles (:perfiles (get-pestaña-activa))]
      (if (< (count perfiles) 2)
          (alert (app-tr @lang :debe-haber-al-menos-dos-perfiles))
          (do (change-ventana ventana-operar-dos "block")
              (gdom/setTextContent perfil-activo-operar-dos-nombre (get-perfil-activo-nombre))
              (crear-lista-de-perfiles operar-dos-select false)
              (cond (= operación :sumar-dos) (gdom/setTextContent operación-dos "+")
                    (= operación :restar-dos) (gdom/setTextContent operación-dos "−")
                    (= operación :multiplicar-dos) (gdom/setTextContent operación-dos "×")
                    (= operación :dividir-dos) (gdom/setTextContent operación-dos "÷")
                    :else nil)
              (.focus operar-dos-select))))))

(defn filtrar-data [perfil lambda-min lambda-max]
  (let [[a b] (:calibración perfil)]
    (filter (fn [punto] (< lambda-min (+ (* a (:x punto)) b)  lambda-max)) (:data-vis perfil))))

(defn operar-dos-perfil-activo [función tag]
  (let [segundo-perfil (get-in @pestañas [:pestañas @pestaña-activa :perfiles (.-value operar-dos-select)])
        segundo-data (obtener-data segundo-perfil) ; data en formato en {:x lambda :y intensidad}
        segundo-data-x (:data-vis segundo-perfil) ; data en formato en {:x x :y intensidad}
        delta (- (:x (second segundo-data-x)) (:x (first segundo-data-x))) ; "paso" en x
        [a-dos b-dos] (:calibración segundo-perfil)
        aux (* a-dos delta)
        lambda-0-dos (:x (first segundo-data))
        perfil-activo (get-perfil-activo)
        [a b] (:calibración perfil-activo)
        data-perfil-activo-encajado (filtrar-data perfil-activo lambda-0-dos (:x (last segundo-data))) ; data-vis en {:x x :y intensidad}
        data-vis-nuevo (mapv (fn [{x :x y :y}]
                               (let [lambda (+ (* a x) b)
                                     j1 (Math/ceil (/ (- lambda lambda-0-dos) aux))
                                     j0 (dec j1)
                                     {lambda-0 :x y0 :y} (segundo-data j0)
                                     {lambda-1 :x y1 :y} (segundo-data j1)
                                     intensidad (cond (= lambda-0 lambda) y0  ; ¿Quizá sea mejor usar directamente el caso general ':else'..?
                                                      (= lambda-1 lambda) y1  ;
                                                      :else (+ y0 (* (- lambda lambda-0)
                                                                     (/ (- y1 y0)
                                                                        (- lambda-1 lambda-0)))))]
                                   {:x x :y (función y intensidad)}))
                             data-perfil-activo-encajado)
        nombre-actual (str (get-perfil-activo-nombre) (app-tr @lang tag) (.-value operar-dos-select))
        nombres-en-pestaña (keys (get-in @pestañas [:pestañas @pestaña-activa :perfiles]))
        nombre (elegir-nombre nombres-en-pestaña nombre-actual false)]
    (agregar-perfil-en-pestaña nombre (assoc perfil-activo :data-vis data-vis-nuevo :etiquetas {}))
    (change-ventana ventana-operar-dos "none")))

(defn operar-dos-perfiles []
  (let [operación (gdom/getTextContent operación-dos)]
    (cond (= operación "+") (operar-dos-perfil-activo + :suma-dos-perfiles)
          (= operación "−") (operar-dos-perfil-activo - :diferencia-dos-perfiles)
          (= operación "×") (operar-dos-perfil-activo * :producto-dos-perfiles)
          (= operación "÷") (operar-dos-perfil-activo / :cociente-dos-perfiles))))

;;; Fin Aritmética

(defn abrir-ventana-borrar-perfil []
  (let [numero-de-perfiles (count (get-in @pestañas [:pestañas @pestaña-activa :perfiles]))]
    (cond (= 0 numero-de-perfiles) (alert (app-tr @lang :no-hay-perfiles-que-borrar))
          (= 1 numero-de-perfiles) (alert (app-tr @lang :el-perfil-activo-no-puede-borrarse))
          :else (do (crear-lista-de-perfiles borrar-perfiles-select false)
                    (change-ventana ventana-borrar-perfil "block")))))

(defn borrar-perfil []
  (confirmar-operación (app-tr @lang :confirmar-borrar-perfil)
                       #(let [perfil-a-borrar (.-value borrar-perfiles-select)]
                           (swap! pestañas update-in [:pestañas @pestaña-activa :perfiles] dissoc perfil-a-borrar)))
  (change-ventana ventana-borrar-perfil "none"))

(defn borrar-etiquetas []
  (if-not (seq (:etiquetas (get-perfil-activo)))
          (alert (app-tr @lang :no-hay-etiquetas-que-borrar))
          (confirmar-operación (app-tr @lang :confirmar-borrar-etiquetas)
                               #(swap! pestañas update-in (get-perfil-activo-key) assoc :etiquetas {}))))

(defonce is-initialized?
  (do (gevents/listen open-fits "change" (fn [this] (abrir-archivo this :fits)))
      (gevents/listen open-annie "change" (fn [this] (abrir-archivo this :annie)))
      (gevents/listen (gdom/getElement "crear-perfil-desde-fits") "click" #(.click open-fits))
      (gevents/listen (gdom/getElement "abrir-pestaña-annie") "click" #(.click open-annie))
      (gevents/listen (gdom/getElement "crear-perfil-desde-dat") "click" abrir-ventana-espectros-dat)
      (gevents/listen espectros-boton-ok "click" espectros-ok)
      (gevents/listen espectros-boton-cancel "click" espectros-cancel)
      (gevents/listen (gdom/getElement "grabar-pestaña-annie") "click" grabar-pestaña-annie)
      ; (gevents/listen (gdom/getElement "svg") "click" grabar-pestaña-svg)
      (gevents/listen cambiar-perfil-ventana-menu "click" abrir-ventana-cambiar-perfil)
      (gevents/listen (gdom/getElement "cerrar-ventana-cambiar-perfil") "click" cerrar-ventana-cambiar-perfil)
      (gevents/listen etiqueta-ok "click" agregar-texto-etiqueta)
      (gevents/listen etiqueta-cancel "click" cancelar-texto-etiqueta)
      (gevents/listen etiqueta-delete "click" borrar-etiqueta)
      (gevents/listen (gdom/getElement "auto-calibracion") "click" abrir-ventana-calibración)
      (gevents/listen calibración-ok "click" calibrar-ok)
      (gevents/listen calibración-cancel "click" calibrar-cancel)
      (gevents/listen copiar-perfil-menu "click" copiar-perfil)
      (gevents/listen pegar-perfil-menu "click" pegar-perfil)
      (gevents/listen perfil-activo-select "change" (fn [e] (cambiar-perfil-activo (.. e -target -value))))
      (gevents/listen boton-zoom-etc "click" change-ventana-zoom-etc)
      (gevents/listen (gdom/getElement "cerrar-ventana-zoom-etc") "click" change-ventana-zoom-etc)
      (gevents/listen (gdom/getElement "optizoom") "click" do-optizoom)
      (gevents/listen (gdom/getElement "zoom-x-menos") "click" (fn [] (reset! plot-width (* (or @plot-width (.-offsetWidth app)) 0.9))))
      (gevents/listen (gdom/getElement "zoom-x-más") "click" (fn [] (reset! plot-width (* (or @plot-width (.-offsetWidth app)) 1.1))))
      (gevents/listen (gdom/getElement "zoom-y-menos") "click" (fn [] (reset! plot-height (* (or @plot-height (.-offsetHeight app)) 0.9))))
      (gevents/listen (gdom/getElement "zoom-y-más") "click" (fn [] (reset! plot-height (* (or @plot-height (.-offsetHeight app)) 1.1))))
      (gevents/listen (gdom/getElement "desplazar-y-abajo") "click" bajar-perfil-activo)
      (gevents/listen (gdom/getElement "desplazar-y-reset") "click" reset-y-perfil-activo)
      (gevents/listen (gdom/getElement "desplazar-y-arriba") "click" subir-perfil-activo)
      (gevents/listen (gdom/getElement "expandir-y") "click" expandir-y-perfil-activo)
      (gevents/listen (gdom/getElement "fact-y-reset") "click" reset-fact-y-perfil-activo)
      (gevents/listen (gdom/getElement "comprimir-y") "click" comprimir-y-perfil-activo)
      (gevents/listen language-selector "change" update-language)
      (gevents/listen (gdom/getElement "controles") "click" (fn [] (change-ventana help-window "block" fondo-gris)))
      (gevents/listen (gdom/getElement "help-window-cerrar") "click" (fn [] (change-ventana help-window "none" fondo-gris)))
      (gevents/listen (gdom/getElement "creditos") "click" (fn [] (change-ventana credits-window "block" fondo-gris)))
      (gevents/listen (gdom/getElement "credits-window-cerrar") "click" (fn [] (change-ventana credits-window "none" fondo-gris)))
      (gevents/listen (gdom/getElement "ok-alert") "click" (fn [] (change-ventana alert-window "none" fondo-gris)))
      (gevents/listen cambiar-color-perfil "input" cambiar-color-perfil-fn)
      (gevents/listen cambiar-ancho-perfil "input" cambiar-ancho-perfil-fn)
      (gevents/listen color-por-defecto-checkbox "change" cambiar-color-perfil-fn)
      (gevents/listen (gdom/getElement "boton-cambiar-nombre-perfil") "click" cambiar-nombre-perfil-fn)
      (gevents/listen (gdom/getElement "normalizacion") "click" normalizar-perfil-activo)
      (gevents/listen (gdom/getElement "sumar-uno") "click" (fn [] (abrir-ventana-operar-uno :sumar-uno)))
      (gevents/listen (gdom/getElement "multiplicar-uno") "click" (fn [] (abrir-ventana-operar-uno :multiplicar-uno)))
      (gevents/listen (gdom/getElement "ok-operar-uno") "click" operar-uno-perfil-activo)
      (gevents/listen (gdom/getElement "cancel-operar-uno") "click" (fn [] (change-ventana ventana-operar-uno "none")))
      (gevents/listen (gdom/getElement "sumar-dos") "click" (fn [] (abrir-ventana-operar-dos :sumar-dos)))
      (gevents/listen (gdom/getElement "restar-dos") "click" (fn [] (abrir-ventana-operar-dos :restar-dos)))
      (gevents/listen (gdom/getElement "multiplicar-dos") "click" (fn [] (abrir-ventana-operar-dos :multiplicar-dos)))
      (gevents/listen (gdom/getElement "dividir-dos") "click" (fn [] (abrir-ventana-operar-dos :dividir-dos)))
      (gevents/listen (gdom/getElement "ok-operar-dos") "click" operar-dos-perfiles)
      (gevents/listen (gdom/getElement "cancel-operar-dos") "click" (fn [] (change-ventana ventana-operar-dos "none")))
      (gevents/listen (gdom/getElement "borrar-perfil") "click" abrir-ventana-borrar-perfil)
      (gevents/listen (gdom/getElement "ok-perfiles-borrar") "click" borrar-perfil)
      (gevents/listen (gdom/getElement "cancel-perfiles-borrar") "click" (fn [] (change-ventana ventana-borrar-perfil "none")))
      (gevents/listen (gdom/getElement "borrar-etiquetas") "click" borrar-etiquetas)
      (gevents/listen (gdom/getElement "cancel-confirmar") "click" (fn [] (change-ventana confirmar-window "none" fondo-gris)))
      (doseq [radio estilos-perfil]
        (gevents/listen radio "change" cambiar-estilo-perfil-fn))
      (doseq [popup popup-forms]
        (gevents/listen popup "mousedown" (fn [e] (reset! mouse {:isDown true
                                                                 :offset {:x (- (.-offsetLeft popup) (.-clientX e))
                                                                          :y (- (.-offsetTop popup) (.-clientY e))}})))
        (gevents/listen popup "mouseup"    (fn [] (swap! mouse assoc :isDown false)))
        (gevents/listen popup "mouseleave" (fn [] (swap! mouse assoc :isDown false)))
        (gevents/listen popup "mousemove" (fn [e] (.preventDefault e)
                                                  (if (:isDown @mouse)
                                                      (let [m-pos {:x (.-clientX e) :y (.-clientY e)}]
                                                         (set! (.. popup -style -left) (str (+ (:x m-pos) (:x (:offset @mouse))) "px"))
                                                         (set! (.. popup -style -top)  (str (+ (:y m-pos) (:y (:offset @mouse))) "px")))))))
      true))

(def nearest-xy (atom {}))
(def nearest-xy-0 (atom {}))
(defn nearest-x [nearest] (get @nearest "x"))
(defn nearest-y [nearest] (get @nearest "y"))

(def pos-mouse-pixels (atom {}))

(def button-izq-pressed? (atom false))
(def button-cen-pressed? (atom false))

(defn calcular-xy-etiqueta [position-in-pixels]
  (let [[x y] [(.-x position-in-pixels) (.-y position-in-pixels)]]
     [(- (:x @pos-mouse-pixels) x 110)
      (- (:y @pos-mouse-pixels) y 95)]))

(defn crear-etiqueta [id x y texto etiqueta]
  (let [pos (conj etiqueta :pos)
        [inc-x inc-y] (get-in @pestañas pos)]
   (vector
     [:> rvis/CustomSVGSeries {:onValueMouseOver (fn [d] (reset! etiqueta-activa etiqueta))
                               :onValueMouseOut  (fn [d] (when-not (or @button-cen-pressed?
                                                                       (= "block" (.. ventana-elementos -style -display)))
                                                             (reset! etiqueta-activa [])))
                               :data [{:x x :y y
                                       :customComponent (fn [_ position-in-pixels]
                                                         (when (and @button-cen-pressed? (= @etiqueta-activa etiqueta))
                                                            (swap! pestañas assoc-in pos (calcular-xy-etiqueta position-in-pixels)))
                                                         (r/as-element [:g {:className "etiqueta"}
                                                                           [:text
                                                                             (map-indexed (fn [i linea]
                                                                                             ^{:key linea}[:tspan {:x inc-x :y (+ inc-y (* i 18))} linea])
                                                                                          texto)]]))}]}]
     [:> rvis/CustomSVGSeries {:data [{:x x :y y
                                       :customComponent (fn []
                                                         (r/as-element [:g {:className "etiqueta cursor-normal"}
                                                                         [:polyline {:points [0 (if (< inc-y 5) -10 5) 0 inc-y inc-x inc-y]
                                                                                     :stroke "black" :fill "none"}]]))}]}])))

(defn open-ventana-elementos [etiqueta]
  (let [perfil-calibrado? (calibrado? (get-perfil-activo))
        texto-en-string (if perfil-calibrado?
                            (->> (get-in @pestañas (conj etiqueta :texto))
                                 (str/join "\n"))
                            (app-tr @lang :etiquetas-no-calibrado))]
    (change-ventana ventana-elementos "block")
    (set! (.-value etiqueta-texto) texto-en-string)
    (set! (.-readOnly etiqueta-texto) (not perfil-calibrado?))
    (when perfil-calibrado? (.select etiqueta-texto))))

(defn colocar-etiqueta []
  (let [perfil (get-perfil-activo)
        inc-y (:inc-y perfil)
        fact-y (:fact-y perfil)
        baricentro (mn/calcular-baricentro (obtener-data perfil) ; Tiene la forma {:x x :y y}
                                           (nearest-x nearest-xy-0) (nearest-x nearest-xy))
        baricentro-no-calibrado (assoc baricentro :x (calcular-x-no-calibrado perfil (:x baricentro)))
        baricentro-no-calibrado (update baricentro-no-calibrado :y #(/ (- % inc-y) fact-y))
        nombre-etiqueta (elegir-nombre (keys (:etiquetas perfil)) "etiqueta" true)
        etiqueta (assoc baricentro-no-calibrado :texto [] :pos [0 18])
        key (conj (get-perfil-activo-key) :etiquetas nombre-etiqueta)]
     (swap! pestañas assoc-in key etiqueta)
     (reset! etiqueta-activa key)
     (open-ventana-elementos key)))

(defn mouse-pressed-within-plot [e dir]
  (when-not (.-ctrlKey e)    ; Con la tecla "Control" se editan etiquetas
    (let [boton (.-button e)]   ; 0: izq, 1: centro, 2: derecho
      (case boton               ; el boton derecho me abre una ventana contextual (supongo que se puede quitar, pero...)
        0 (do (when (= dir :up) (colocar-etiqueta))
              (reset! nearest-xy-0 (if (= dir :down) @nearest-xy {}))
              (swap! button-izq-pressed? not))
        1 (reset! button-cen-pressed? (if (= dir :down) true false)) ; Si uso <<swap! not>>, hay problemas al salir y entrar al gráfico...
        2))))

(defn mouse-moved [e]
  (when (or @button-izq-pressed? @button-cen-pressed?)
    (reset! pos-mouse-pixels {:x (.-clientX e) :y (.-clientY e)})))

(def line-style {:fill "none" :strokeLinejoin "round" :strokeLinecap "round"})
(def axis-style {:line {:stroke "#333"}
                 :ticks {:stroke "#999"}
                 :text {:stroke "none"
                        :fill "#333"}})

(defn line-chart []
  (let [width (or @plot-width (.-offsetWidth app))
        height (or @plot-height (.-offsetHeight app))
        perfil-activo (get-perfil-activo)
        perfiles-pestaña-activa (:perfiles (get-pestaña-activa))
        x-min (calcular-x-calibrado perfil-activo (:x (first (:data-vis perfil-activo))))
        x-max (calcular-x-calibrado perfil-activo (:x (last  (:data-vis perfil-activo))))]
   [:div#graph ;{:style {:heigth (.. app -style -height)}}
    (into
     [:> rvis/XYPlot
      {:margin {:left 100 :right 50 :top 20}
       :width width :height height :onMouseDown  (fn [e] (mouse-pressed-within-plot e :down))
                                   :onMouseUp    (fn [e] (mouse-pressed-within-plot e :up))
                                   :onMouseMove  (fn [e] (mouse-moved e))
                                   :onMouseLeave (fn [e] ;;(reset! etiqueta-activa [])
                                                   (reset! button-cen-pressed? false))
                                   :onClick      (fn [e] (when (seq @etiqueta-activa)
                                                            (open-ventana-elementos @etiqueta-activa)))}
      [:> rvis/VerticalGridLines {:style axis-style}]
      [:> rvis/HorizontalGridLines {:style axis-style}]
      [:> rvis/XAxis {:tickSizeInner 0 :tickSizeOuter 6 :style axis-style}]
      [:> rvis/YAxis {:tickSizeInner 0 :tickSizeOuter 6 :style axis-style}]

      [:> rvis/Crosshair {:values [{:x (nearest-x nearest-xy) :y 0}] :strokeStyle "dashed" :strokeDasharray  "10,10"
                          :style {:line {:background "black" :opacity 1 :strokeDasharray "10,10"}}}
         [:div]]
      [:> rvis/Crosshair {:values [{:x (nearest-x nearest-xy-0) :y 0}]
                          :style {:line {:background "black" :opacity (if @button-izq-pressed? 1 0)}}}
         [:div]]
      [:> rvis/DiscreteColorLegend {:style {:position "fixed" :left 110 :top (+ alto-header 10)} 
                                    :items (mapv (fn [[name perfil]] (conj {:title name :strokeWidth (or (:width perfil) 1)}
                                                                           (if-let [color (:color perfil)] [:color color])
                                                                           (if-let [estilo (:dasharray perfil)] [:strokeDasharray estilo])))
                                                 perfiles-pestaña-activa)}]
      (doall (for [[id perfil] perfiles-pestaña-activa]
               ^{:key (str id)} [:> rvis/LineSeries (conj {:data (filtrar-dominio (obtener-data perfil) x-min x-max)
                                                              :style {:fill "none" :strokeLinejoin "round"}
                                                           :strokeWidth (or (:width perfil) 1)
                                                           :onNearestX (fn [e] (reset! nearest-xy (js->clj e)))}
                                                           (if-let [color (:color perfil)]
                                                              [:color color])
                                                           (if-let [estilo (:dasharray perfil)]
                                                              [:strokeDasharray estilo]))]))]
     (let [pestaña-perfil-etiqueta-nombre (conj (get-perfil-activo-key) :etiquetas)
           inc-y (:inc-y (get-perfil-activo))
           fact-y (:fact-y (get-perfil-activo))]
        (mapcat (fn [[id {:keys [x y texto]}]]
                   (let [xc (calcular-x-calibrado perfil-activo x)
                         texto-a-mostrar (concat [(.toFixed xc 1)] (if (calibrado? perfil-activo) texto))]
                     (crear-etiqueta id xc (+ (* y fact-y) inc-y) texto-a-mostrar (conj pestaña-perfil-etiqueta-nombre id))))
                (:etiquetas perfil-activo))))]))

(defn pestaña-activa? [nombre]
  (= nombre @pestaña-activa))

(defn cerrar-pestaña
  ([nombre]
    (confirmar-operación (app-tr @lang :confirmar-borrar-pestaña) #(cerrar-pestaña nombre true)))
  ([nombre forced]
    (swap! pestañas update-in [:pestañas] dissoc nombre)
    (when (= nombre @pestaña-activa)
      (swap! pestañas assoc :pestaña-activa (if-let [pestañas-restantes (keys (:pestañas @pestañas))]
                                              (first pestañas-restantes))))))

(defn crear-botones []
 [:div
   (if-not @pestaña-activa
      [:button {:className "boton-vacio"} "Vacío"]
      (doall (for [nombre (keys (:pestañas @pestañas))]
               ^{:key (str "pestaña-" nombre)}
               [:button {:id (str "pestaña-" nombre) :className (if (pestaña-activa? nombre) "active")
                         :on-click (fn [] (swap! pestañas assoc :pestaña-activa nombre))}
                        nombre
                        [:span.close-tab {:on-click (fn [e]
                                                      (cerrar-pestaña nombre)
                                                      (.stopPropagation e))}
                                         (goog.string/unescapeEntities "&times;")]])))])

(ratom/run!
  (if @pestaña-activa
      (do
        (crear-lista-de-perfiles perfil-activo-select true)
        (actualizar-dispersión-span)
        (actualizar-ventana-cambiar-perfil)
        (set! (.. menu-perfil-activo -style -display) "flex"))
      (set! (.. menu-perfil-activo -style -display) "none")))

(defn mount-elements []
  (rdom/render [crear-botones] tabs)
  (rdom/render [line-chart] app))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-elements)
(do-optizoom)

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (mount-elements))
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

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
   [lopezsolerluis.save-load-file :as save :refer [write-pestaña read-pestaña]]))

(enable-console-print!)
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
(def language-selector (gdom/getElement "language"))
;; Para que el gráfico pueda hacer "scroll" dentro de un div fijo... casi hacker!
(set! (.. app -style -height)
      (str "calc( 100vh - " (.-offsetHeight menu-principal) "px - " (.-offsetHeight tabs) "px )"))

(defn get-perfil-activo-key []
  (let [pestaña-activa-nombre (:pestaña-activa @pestañas)
        perfil-activo-nombre (get-in @pestañas [:pestañas pestaña-activa-nombre :perfil-activo])]
    [:pestañas pestaña-activa-nombre :perfiles perfil-activo-nombre]))

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

(defn crear-lista-de-perfiles []
  (let [pestaña-activa (get-pestaña-activa)
        perfiles-nombres (keys (:perfiles pestaña-activa))
        perfil-activo-nombre (:perfil-activo pestaña-activa)]
    (gdom/removeChildren perfil-activo-select)
    (doseq [nombre perfiles-nombres]
      (let [option (.createElement js/document "option")]
        (set! (.-value option) nombre)
        (set! (.-innerHTML option) nombre)
        (.appendChild perfil-activo-select option)))
    (set! (.-value perfil-activo-select) perfil-activo-nombre)))

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
   (gdom/setTextContent (gdom/getElement "perfiles-label") (str (app-tr lang :ventana-zoom-etc/perfil-activo) ":"))
   (doseq [key-1 [:menu :ventana-etiqueta :ventana-calibración :ventana-espectros :ventana-zoom-etc]]
     (doseq [key-2 (-> translations :es key-1 keys)]
       (let [el (gdom/getElement (name key-2))]
         (gdom/setTextContent el (app-tr lang (keyword (name key-1) key-2))))))))

(set! (.-value language-selector) (getLanguage))
(defn update-language [evt]
  (reset! lang (.. evt -target -value))
  (traducir))

(traducir)
;; end of translation functions

(defn alert [mensaje]
  (js/alert mensaje))

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
        [a b] (calcular-calibración mínimo máximo 0 1)] ;el algoritmo para cacular la calibración es el mismo: una función lineal
    (map (fn [n] (+ (* a n) b)) perfil-2d)))

(defn crear-data-para-vis [perfil-2d]
  (mapv (fn [x y] {:x x :y y}) (range) perfil-2d))

(defn crear-pestaña
  ([nombre-posible data-para-vis] (crear-pestaña nombre-posible data-para-vis []))
  ([nombre-posible data-para-vis calibración]
   (let [nombre (elegir-nombre (keys (:pestañas @pestañas)) nombre-posible false)]
     (swap! pestañas assoc-in [:pestañas nombre] {:perfil-activo nombre})
     (swap! pestañas assoc-in [:pestañas nombre :perfiles nombre]  ; pestaña perfil
                           {:data-vis data-para-vis :calibración calibración :inc-y 0 :etiquetas {}})
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

(defn calibrado? [perfil]
  (seq (:calibración perfil)))

(defn change-ventana [ventana state]  ; state es "block" o "none"
  (set! (.. ventana -style -display) state)
  (set! (.. fondo-transparente -style -display) state))

(defn confirmar-operación [texto]
  (js/window.confirm texto))

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

(defn pegar-perfil []
  (if (empty? @portapapeles)
      (alert (app-tr @lang :portapapeles-vacío))
      (let [perfil-activo (get-perfil-activo)]
        (if-not (calibrado? perfil-activo)
                (alert (app-tr @lang :perfil-no-calibrado-no-admite-pegado))
                (let [pestaña-activa-nombre (:pestaña-activa @pestañas)
                      nombre-copiado (first @portapapeles)
                      nombres-en-pestaña (keys (get-in @pestañas (butlast (get-perfil-activo-key))))
                      nombre (elegir-nombre  nombres-en-pestaña nombre-copiado false)
                      perfil-pegado (second @portapapeles)]
                  (swap! pestañas assoc-in [:pestañas pestaña-activa-nombre :perfiles nombre] perfil-pegado))))))

(defn agregar-texto-etiqueta []
  (let [perfil-activo (get-perfil-activo)]
    (when (calibrado? perfil-activo)
      (let [texto (str/split-lines (.-value etiqueta-texto))]
        (swap! pestañas assoc-in (conj @etiqueta-activa :texto) texto)))
    (change-ventana ventana-elementos "none")))
(defn cancelar-texto-etiqueta []
  (change-ventana ventana-elementos "none"))
(defn borrar-etiqueta []
  (when (confirmar-operación (app-tr @lang :confirmar-borrar-etiqueta))
    (swap! pestañas update-in (pop @etiqueta-activa) dissoc (last @etiqueta-activa)))
  (change-ventana ventana-elementos "none"))

(defn filtrar-dominio [data x-min x-max]
  (filter (fn [punto] (<= x-min (:x punto) x-max)) data))

(defn calibrar-data-vis [data a b]
  (mapv (fn [{:keys [x y]}] {:x (+ (* a x) b) :y y}) data))
(defn obtener-data [perfil]
  (if (calibrado? perfil)
      (let [[a b] (:calibración perfil)
            inc-y (:inc-y perfil)]
        (mapv (fn [{:keys [x y]}] {:x (+ (* a x) b) :y (+ y inc-y)}) (:data-vis perfil)))
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
  ;(desplazar-perfil-y (get-perfil-activo-key) 0.05))
(defn bajar-perfil-activo []
  (swap! pestañas update-in (conj (get-perfil-activo-key) :inc-y) #(- % 0.05)))
  ;(desplazar-perfil-y (get-perfil-activo-key) -0.05))
(defn reset-y-perfil-activo []
  (swap! pestañas assoc-in (conj (get-perfil-activo-key) :inc-y) 0))

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

(defn cambiar-perfil-activo [nombre]
  (swap! pestañas assoc-in [:pestañas @pestaña-activa :perfil-activo] nombre))

(defn change-ventana-zoom-etc []
  (if (= (.. ventana-zoom-etc -style -display) "block")
      (do
        (set! (.. ventana-zoom-etc -style -display) "none")
        (set! (.. boton-zoom-etc -style -borderStyle) "outset"))
      (do
        (set! (.. ventana-zoom-etc -style -display) "block")
        (set! (.. boton-zoom-etc -style -borderStyle) "inset"))))

(defn do-optizoom []
   (reset! plot-height 10) ; No estoy seguro de si esto es necesario;
   (reset! plot-width 10)  ; ni siquiera si es útil
   (reset! plot-width  (.-offsetWidth app))
   (reset! plot-height (.-offsetHeight app)))

(def mouse (atom {:isDown false :offset {:x 0 :y 0}}))

(defonce is-initialized?
  (do (gevents/listen open-fits "change" (fn [this] (abrir-archivo this :fits)))
      (gevents/listen open-annie "change" (fn [this] (abrir-archivo this :annie)))
      (gevents/listen (gdom/getElement "crear-perfil-desde-fits") "click" #(.click open-fits))
      (gevents/listen (gdom/getElement "abrir-pestaña-annie") "click" #(.click open-annie))
      (gevents/listen (gdom/getElement "crear-perfil-desde-dat") "click" abrir-ventana-espectros-dat)
      (gevents/listen espectros-boton-ok "click" espectros-ok)
      (gevents/listen espectros-boton-cancel "click" espectros-cancel)
      (gevents/listen (gdom/getElement "grabar-pestaña-annie") "click" grabar-pestaña-annie)
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
      (gevents/listen (gdom/getElement "zoom-x-menos") "click" (fn [] (swap! plot-width * 0.9)))
      (gevents/listen (gdom/getElement "zoom-x-más") "click" (fn [] (swap! plot-width * 1.1)))
      (gevents/listen (gdom/getElement "zoom-y-menos") "click" (fn [] (swap! plot-height * 0.9)))
      (gevents/listen (gdom/getElement "zoom-y-más") "click" (fn [] (swap! plot-height * 1.1)))
      (gevents/listen (gdom/getElement "desplazar-y-abajo") "click" bajar-perfil-activo)
      (gevents/listen (gdom/getElement "desplazar-y-reset") "click" reset-y-perfil-activo)
      (gevents/listen (gdom/getElement "desplazar-y-arriba") "click" subir-perfil-activo)
      (gevents/listen language-selector "change" update-language)
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
        baricentro (mn/calcular-baricentro (obtener-data perfil) ; Tiene la forma {:x x :y y}
                                           (nearest-x nearest-xy-0) (nearest-x nearest-xy))
        baricentro-no-calibrado (assoc baricentro :x (calcular-x-no-calibrado perfil (:x baricentro)))
        baricentro-no-calibrado (update baricentro-no-calibrado :y #(- % inc-y))
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
  (let [width (or @plot-width 0)
        height (or @plot-height 0)
        perfil-activo (get-perfil-activo)
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
      (let [perfiles-pestaña-activa (get-in @pestañas [:pestañas (:pestaña-activa @pestañas) :perfiles])]
         (doall (for [[id perfil] perfiles-pestaña-activa]
                     ^{:key (str id)} [:> rvis/LineSeries {:data (filtrar-dominio (obtener-data perfil) x-min x-max) :style {:fill "none"}
                                                           :strokeWidth 1
                                                           :onNearestX (fn [e]
                                                                        (reset! nearest-xy (js->clj e)))}])))]
     (let [pestaña-perfil-etiqueta-nombre (conj (get-perfil-activo-key) :etiquetas)
           inc-y (:inc-y (get-perfil-activo))]
        (mapcat (fn [[id {:keys [x y texto]}]]
                   (let [xc (calcular-x-calibrado perfil-activo x)
                         texto-a-mostrar (concat [(.toFixed xc 1)] (if (calibrado? perfil-activo) texto))]
                     (crear-etiqueta id xc (+ y inc-y) texto-a-mostrar (conj pestaña-perfil-etiqueta-nombre id))))
                (:etiquetas perfil-activo))))]))

(defn pestaña-activa? [nombre]
  (= nombre (:pestaña-activa @pestañas)))

(defn crear-botones []
 [:div
   (if-not @pestaña-activa
      [:button {:className "boton-vacio"} "Vacío"]
      (doall (for [nombre (keys (:pestañas @pestañas))]
               ^{:key (str "pestaña-" nombre)}
               [:button {:id (str "pestaña-" nombre) :className (if (pestaña-activa? nombre) "active")
                         :on-click (fn[] (swap! pestañas assoc :pestaña-activa nombre))}
                        nombre])))])

(ratom/run!
  (if @pestaña-activa
      (do
        (crear-lista-de-perfiles)
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

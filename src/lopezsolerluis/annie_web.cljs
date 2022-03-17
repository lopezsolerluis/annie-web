(ns ^:figwheel-hooks lopezsolerluis.annie-web
  (:require
   [goog.dom :as gdom]
   [goog.events :as gevents]
   [reagent.core :as r :refer [atom]]
   [reagent.dom :as rdom]
   [cljsjs.react-vis :as rvis]
   [clojure.string :as str]
   [lopezsolerluis.traducciones :as trad :refer [app-tr translations]]
   [lopezsolerluis.fits :as fits]
   [lopezsolerluis.metodos-numericos :as mn]
   [lopezsolerluis.save-file :as save :refer [download-object-as-json]]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload
(defonce pestañas (atom {}))  ; ¿defonce o def..?
(defonce perfil-activo (atom "")) ;idem
(defonce pestaña-activa (atom ""))
(def etiqueta-activa (atom []))

(def icono-espera (gdom/getElement "loader"))
(def fondo-gris (gdom/getElement "fondogris"))
(def ventana-elementos (gdom/getElement "ventana-elementos"))
(def fondo-transparente (gdom/getElement "fondoblanco"))
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

(defn encender-espera []
  (set! (.. icono-espera -style -display) "block")
  (set! (.. fondo-gris -style -display) "block"))
(defn apagar-espera []
  (set! (.. icono-espera -style -display) "none")
  (set! (.. fondo-gris -style -display) "none"))

;; Translation functions
(defn getLanguage []
  (-> (or (.-language js/navigator) (.-userLanguage js/navigator) "en")
      (subs 0 2)))
(def lang (r/atom (keyword (getLanguage))))
(defn traducir
  ([] (traducir @lang))
  ([lang]
    (doseq [key-1 [:menu :ventana-etiqueta]]
      (doseq [key-2 (-> translations :es key-1 keys)]
        (let [el (gdom/getElement (name key-2))]
          (gdom/setTextContent el (app-tr lang (keyword (name key-1) key-2))))))))
;; end of translation functions

(defn alert [mensaje]
  (js/alert mensaje))

(defn crear-perfil [fits-file]
  (let [data (:data fits-file)]
          ;(apply map + data))) ; Para sumar las columnas (tarda mucho más)
         (map #(reduce + %) data)))  ; Suma sobre las filas, porque el archivo "fits" lo creé 'traspuesto'...¡casi hacker!

(defn crear-data-para-vis [perfil-2d]
  (mapv (fn [x y] {:x x :y y}) (range) perfil-2d))

(defn procesar-archivo-fits [fits-file]
  (if (= fits-file :fits-no-simple)
      (js/alert (app-tr @lang :fits-no-valido))
      (let [perfil (crear-perfil fits-file)
            data-para-vis (crear-data-para-vis perfil)
            nombre (:nombre-archivo fits-file)]
         (reset! pestaña-activa nombre)
         (reset! perfil-activo nombre)
         (swap! pestañas assoc-in [@pestaña-activa @perfil-activo]
                                  {:data-vis data-para-vis :calibración [] :etiquetas {}})))
  (apagar-espera))

(defn calibrado? [perfil]
  (seq (:calibración perfil)))

(defn input-fits-file []
  [:input {:type "file" :id "fits"
           :on-change (fn [this]
                        (if-not (= "" (-> this .-target .-value))
                          (let [^js/File file (-> this .-target .-files (aget 0))]
                            (encender-espera)
                            (fits/read-fits-file file procesar-archivo-fits)))
                          (set! (-> this .-target .-value) ""))}])

(defn change-ventana [ventana state]  ; state es "block" o "none"
  (set! (.. ventana -style -display) state)
  (set! (.. fondo-transparente -style -display) state))

(defn confirmar-operación [texto]
  (js/window.confirm texto))

(defn agregar-texto-etiqueta []
  (let [perfil-calibrado? (calibrado? (get-in @pestañas [@pestaña-activa @perfil-activo]))
        texto (if perfil-calibrado?
                  (str/split-lines (.-value etiqueta-texto))
                  [(-> (get-in @pestañas (conj @etiqueta-activa :x))
                       (.toFixed 1)
                       str)])]
    (swap! pestañas assoc-in (conj @etiqueta-activa :texto) texto)
    (change-ventana ventana-elementos "none")))
(defn cancelar-texto-etiqueta []
  (change-ventana ventana-elementos "none"))
(defn borrar-etiqueta []
  (if (confirmar-operación (app-tr @lang :confirmar-borrar-etiqueta))
      (swap! pestañas update-in (pop @etiqueta-activa) dissoc (last @etiqueta-activa)))
  (change-ventana ventana-elementos "none"))

(defn calibrar-data-vis [data a b]
  (mapv (fn [{:keys [x y]}] {:x (+ (* a x) b) :y y}) data))
(defn obtener-data [perfil]
  (if (calibrado? perfil)
      (let [[a b] (:calibración perfil)]
        (mapv (fn [{:keys [x y]}] {:x (+ (* a x) b) :y y}) (:data-vis perfil)))
      (:data-vis perfil)))

(defn abrir-ventana-calibración []
  (let [ultimas-etiquetas (take-last 2 (get-in @pestañas [@pestaña-activa @perfil-activo :etiquetas]))]
    (if-not (= 2 (count ultimas-etiquetas))
            (alert (app-tr @lang :debería-seleccionar-dos-líneas))
            (let [baricentros (map :x (vals ultimas-etiquetas))
                  x1 (apply min baricentros)
                  x2 (apply max baricentros)]
              (change-ventana ventana-calibración "block")
              (set! (.-value x1-calibración-number) x1)
              (set! (.-value x2-calibración-number) x2)))))

(defn calcular-calibración [x1 x2 lambda1 lambda2]
  (let [a (/ (- lambda2 lambda1) (- x2 x1)) ;(/ (- l2 l1) (- x2 x1))
        b (/ (- (* lambda1 x2) (* lambda2 x1)) (- x2 x1))] ; (/ (- (* l1 x2) (* l2 x1)) (- x2 x1))))
     [a b]))

(defn calibrar-ok []
  (let [lambda1 (js/parseFloat (.-value lambda1-calibración-number))
        lambda2 (js/parseFloat (.-value lambda2-calibración-number))]
        (if (or (js/isNaN lambda1) (js/isNaN lambda2))
            (alert (app-tr @lang :deben-ingresarse-dos-lambdas))
            (let [x1 (js/parseFloat (.-value x1-calibración-number)) ; verificar que son válidos (?)
                  x2 (js/parseFloat (.-value x2-calibración-number))
                  params (calcular-calibración x1 x2 lambda1 lambda2)]
              (swap! pestañas assoc-in [@pestaña-activa @perfil-activo :calibración] params)
              (change-ventana ventana-calibración "none")))))
(defn calibrar-cancel []
  (change-ventana ventana-calibración "none"))

(defonce is-initialized?
  (do (gevents/listen (gdom/getElement "crear-perfil-desde-fits") "click"
                  #(.click (gdom/getElement "fits")))
      (gevents/listen (gdom/getElement "grabar-pestaña-annie-como") "click"
                  (fn [] (download-object-as-json (clj->js (get-in pestañas [@pestaña-activa @perfil-activo]))
                                                  (str @pestaña-activa ".annie"))))
      (gevents/listen etiqueta-ok "click" agregar-texto-etiqueta)
      (gevents/listen etiqueta-cancel "click" cancelar-texto-etiqueta)
      (gevents/listen etiqueta-delete "click" borrar-etiqueta)
      (gevents/listen (gdom/getElement "auto-calibracion") "click" abrir-ventana-calibración)
      (gevents/listen calibración-ok "click" calibrar-ok)
      (gevents/listen calibración-cancel "click" calibrar-cancel)
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
      (- (:y @pos-mouse-pixels) y 55)]))

(defn crear-etiqueta [id x y texto etiqueta]
  (let [pos (conj etiqueta :pos)
        [inc-x inc-y] (get-in @pestañas pos)]
  (vector
   ;^{:key id}
    [:> rvis/CustomSVGSeries {:onValueMouseOver (fn [d] (reset! etiqueta-activa etiqueta))
                              :onValueMouseOut  (fn [d] (if-not (or @button-cen-pressed?
                                                                (= "block" (.. ventana-elementos -style -display)))
                                                          (reset! etiqueta-activa [])))
                              :data [{:x x :y y
                                :customComponent (fn [_ position-in-pixels]
                                  (if (and @button-cen-pressed? (= @etiqueta-activa etiqueta))
                                    (swap! pestañas assoc-in pos (calcular-xy-etiqueta position-in-pixels)))
                                  (r/as-element [:g {:className "etiqueta"}
                                                      [:text
                                                        (map-indexed (fn [i linea]
                                                                        ^{:key linea}[:tspan {:x inc-x :y (+ inc-y (* i 18))} linea])
                                                                      texto)]]))}]}]
                                                        ; (for [i (range (count texto))]
                                                        ;   [:tspan {:x inc-x :y (+ inc-y (* i 18))} (get texto i)])
     ;^{:key (str key "line")}
     [:> rvis/CustomSVGSeries {:data [{:x x :y y
                                :customComponent (fn []
                                   (r/as-element [:g {:className "etiqueta cursor-normal"}
                                                   [:polyline {:points [0 (if (< inc-y 5) -10 5) 0 inc-y inc-x inc-y]
                                                               :stroke "black" :fill "none"}]]))}]}])))

(defn elegir-nombre [nombres-usados sufijo]
   (let [nombres-set (set nombres-usados)]
      (loop [n 1]
         (let [nombre (keyword (str sufijo n))]
           (if-not (nombres-set nombre)
                   (keyword nombre)
                   (recur (inc n)))))))

(defn open-ventana-elementos [etiqueta]
  (let [perfil-calibrado? (calibrado? (get-in @pestañas [@pestaña-activa @perfil-activo]))
        texto-en-string (if perfil-calibrado?
                            (->> (get-in @pestañas (conj etiqueta :texto))
                                 (str/join "\n"))
                            (app-tr @lang :etiquetas-no-calibrado))]
    (change-ventana ventana-elementos "block")
    (set! (.-value etiqueta-texto) texto-en-string)
    (set! (.-readOnly etiqueta-texto) (not perfil-calibrado?))
    (if perfil-calibrado? (.select etiqueta-texto))))

(defn colocar-etiqueta []
  (let [perfil (get-in @pestañas [@pestaña-activa @perfil-activo])
        baricentro (mn/calcular-baricentro (:data-vis perfil) ; Tiene la forma {:x x :y y}
                                           (nearest-x nearest-xy-0) (nearest-x nearest-xy))
        nombre (elegir-nombre (keys (:etiquetas perfil)) "etiqueta-")
        texto [(.toFixed (:x baricentro) 1)]
        etiqueta (assoc baricentro :texto texto :pos [0 18])
        key [@pestaña-activa @perfil-activo :etiquetas nombre]]
     (swap! pestañas assoc-in key etiqueta)
     (reset! etiqueta-activa key)
     (open-ventana-elementos key)))

(defn mouse-pressed [e dir]
  (if-not (.-ctrlKey e)    ; Con la tecla "Control" se editan etiquetas
    (let [boton (.-button e)]   ; 0: izq, 1: centro, 2: derecho
      (case boton               ; el boton derecho me abre una ventana contextual (supongo que se puede quitar, pero...)
        0 (do (if (= dir :up) (colocar-etiqueta))
              (reset! nearest-xy-0 (if (= dir :down) @nearest-xy {}))
                (swap! button-izq-pressed? not))
        1 (swap! button-cen-pressed? not)
        2 ))))

(defn mouse-moved [e]
  (if (or @button-izq-pressed? @button-cen-pressed?)
    (reset! pos-mouse-pixels {:x (.-clientX e) :y (.-clientY e)})))

(def line-style {:fill "none" :strokeLinejoin "round" :strokeLinecap "round"})
(def axis-style {:line {:stroke "#333"}
                 :ticks {:stroke "#999"}
                 :text {:stroke "none"
                 :fill "#333"}})

(defn line-chart []
  [:div.graph
  (into
  [:> rvis/FlexibleXYPlot
   {:margin {:left 100 :right 50 :top 20} :onMouseDown (fn [e] (mouse-pressed e :down))
                                          :onMouseUp   (fn [e] (mouse-pressed e :up))
                                          :onMouseMove (fn [e] (mouse-moved e))
                                          :onClick     (fn [e] (if (seq @etiqueta-activa)
                                                                   (open-ventana-elementos @etiqueta-activa)))}
   [:> rvis/VerticalGridLines {:style axis-style}]
   [:> rvis/HorizontalGridLines {:style axis-style}]
   [:> rvis/XAxis {:tickSizeInner 0 :tickSizeOuter 6 :style axis-style}]
   [:> rvis/YAxis {:tickSizeInner 0 :tickSizeOuter 6 :style axis-style}]

   [:> rvis/Crosshair {:values [{:x (nearest-x nearest-xy) :y 0}] :strokeStyle "dashed" :strokeDasharray  "10,10"
                       :style {:line {:background "black" :opacity 1 :strokeDasharray "10,10" }}}
      [:div]]
   [:> rvis/Crosshair {:values [{:x (nearest-x nearest-xy-0) :y 0}]
                       :style {:line {:background "black" :opacity (if @button-izq-pressed? 1 0)}}}
      [:div]]
   (doall (for [[id perfil] (get @pestañas @pestaña-activa)]
               ^{:key (str id)} [:> rvis/LineSeries {:data (obtener-data perfil) :style {:fill "none"}
                                                     :strokeWidth 1
                                                     :onNearestX (fn [e]
                                                            (reset! nearest-xy (js->clj e)))}]))
   ; (let [perfil (get-in @pestañas [@pestaña-activa @perfil-activo])]
   ;    (doall (for [[id {:keys [x y texto]}] (:etiquetas perfil)
   ;                 :let [texto-a-mostrar (if (:calibrado? perfil) texto [(.toFixed x 1)])]]
   ;              (crear-etiqueta id x y texto-a-mostrar [@pestaña-activa @perfil-activo :etiquetas id]))))
   ]
      (let [perfil (get-in @pestañas [@pestaña-activa @perfil-activo])]
          (mapcat (fn [[id {:keys [x y texto]}]]
                    (let [texto-a-mostrar (if (calibrado? perfil) texto [(.toFixed x 1)])]
                      (crear-etiqueta id x y texto-a-mostrar [@pestaña-activa @perfil-activo :etiquetas id])))
                  (:etiquetas perfil)))
   )])

(defn app-scaffold []
  [:div.todo
   [input-fits-file]
   [line-chart]])

(defn get-app-element []
  (gdom/getElement "app"))

(defn mount [el]
  (rdom/render [app-scaffold] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

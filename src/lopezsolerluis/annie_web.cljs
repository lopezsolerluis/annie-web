(ns ^:figwheel-hooks lopezsolerluis.annie-web
  (:require
   [goog.dom :as gdom]
   [goog.events :as gevents]
   [reagent.core :as r :refer [atom]]
   [reagent.dom :as rdom]
   [cljsjs.react-vis :as rvis]
   [lopezsolerluis.traducciones :as trad :refer [app-tr translations]]
   [lopezsolerluis.fits :as fits]
   [lopezsolerluis.metodos-numericos :as mn]
   [lopezsolerluis.save-file :as save :refer [download-object-as-json]]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload
(defonce perfiles (atom {}))  ; ¿defonce o def..?
(def icono-espera (gdom/getElement "loader"))
(def fondo-gris (gdom/getElement "fondogris"))

(defn encender-espera []
  (set! (-> icono-espera .-style .-display) "block")
  (set! (-> fondo-gris .-style .-display) "block"))
(defn apagar-espera []
  (set! (-> icono-espera .-style .-display) "none")
  (set! (-> fondo-gris .-style .-display) "none"))

;; Translation functions
(defn getLanguage []
  (-> (or (.-language js/navigator) (.-userLanguage js/navigator) "en")
      (subs 0 2)))
(def lang (r/atom (keyword (getLanguage))))
(def nombres-menu (keys (-> translations :en :menu)))
(defn traducir
  ([] (traducir @lang))
  ([lang]
    (doseq [nombre nombres-menu]
      (let [el (gdom/getElement (name nombre))]
        (gdom/setTextContent el (app-tr lang (keyword "menu" nombre)))))))
;; end of translation functions

(defn crear-perfil [fits-file]
  (let [data (:data fits-file)]
          ;(apply map + data))) ; Para sumar las columnas (tarda mucho más)
         (map #(reduce + %) data)))  ; Suma sobre las filas, porque el archivo "fits" lo creé 'traspuesto'

(defn crear-data-para-vis [perfil-2d]
  (mapv (fn [x y] {:x x :y y}) (range) perfil-2d))

(defn procesar-archivo [fits-file]
  (if (= fits-file :fits-no-simple)
      (js/alert (app-tr @lang :fits-no-valido))
      (let [perfil (crear-perfil fits-file)
            data-para-vis (crear-data-para-vis perfil)
            nombre (:nombre-archivo fits-file)]
         (swap! perfiles assoc nombre {:nombre nombre :data-vis data-para-vis})))
  (apagar-espera))

(defn input-fits-file []
  [:input {:type "file" :id "fits"
           :on-change (fn [this]
                        (if-not (= "" (-> this .-target .-value))
                          (let [^js/File file (-> this .-target .-files (aget 0))]
                            (encender-espera)
                            (fits/read-fits-file file procesar-archivo)))
                          (set! (-> this .-target .-value) ""))}])
(defonce is-initialized?
  (do
    (gevents/listen (gdom/getElement "crear-perfil-desde-fits") "click" #(.click (gdom/getElement "fits")))
    true))


(def nearest-xy (atom {}))
(def nearest-xy-pressed (atom {}))
(defn nearest-x [nearest] (get @nearest "x"))
(defn nearest-y [nearest] (get @nearest "y"))

(def pos-mouse-pixels (r/atom {}))

(def button-pressed? (r/atom false))
(defn mouse-pressed [e dir]
  ;;(js/console.log (.-clientX e))
  (let [boton (.-button e)] ; 0: izq, 1: centro, 2: derecho
    (when (= boton 0)         ; el boton derecho me abre una ventana contextual (supongo que se puede quitar, pero...)
      (reset! nearest-xy-pressed (if (= dir :down) @nearest-xy {}))
      (swap! button-pressed? not))))
(defn mouse-moved [e]
  (if @button-pressed?
    (reset! pos-mouse-pixels {:x (.-clientX e) :y (.-clientY e)})))

(defn calcular-xy-etiqueta []
  [(if (and button-pressed? ) (:x @pos-mouse-pixels) 0)
   (if (and button-pressed? ) (:y @pos-mouse-pixels) 0)]
  )

(let [mouse-over? (atom false)]
  (defn crear-etiqueta [x y]
    ;;(js/console.log "Crear" @button-pressed?  (:x @pos-mouse-pixels))
    ^{:key "etiq"}
    [:> rvis/CustomSVGSeries {:onValueMouseOver (fn [d] (reset! mouse-over? true))
                              :onValueMouseOut  (fn [d] (reset! mouse-over? false))
                              :data [{:x x :y y ; :style {:cursor "wait"} no funciona... (?)
                                :customComponent (fn [_ position-in-pixels]
                                  (let [[inc-x inc-y] (calcular-xy-etiqueta)]
                                   (r/as-element [:g {:className "etiqueta"}
                                                    ;[:circle {:cx 0 :cy 0 :r 20 :fill "orange"}]
                                                    [:text
                                                      [:tspan {:x (- inc-x (.-x position-in-pixels) 110) :y (- inc-y (.-y position-in-pixels) 20 35)} "Hidrógeno "]
                                                      [:tspan {:x inc-x :y "1em"} "Alfa" (str @mouse-over?)]]])))}]

                              ;:id "mi-etiqueta"
                                }]
      ))

(def line-style {:fill "none" :strokeLinejoin "round" :strokeLinecap "round"})
(def axis-style {:line {:stroke "#333"}
                 :ticks {:stroke "#999"}
                 :text {:stroke "none"
                 :fill "#333"}})

(def etiquetas (atom []))

(defn line-chart []
  [:div.graph
  [:> rvis/FlexibleXYPlot
   {:margin {:left 100 :right 50 :top 20} :onMouseDown (fn [e] (mouse-pressed e :down))
                                          :onMouseUp   (fn [e] (mouse-pressed e :up))
                                          :onMouseMove (fn [e] (mouse-moved e))}
   [:> rvis/VerticalGridLines {:style axis-style}]
   [:> rvis/HorizontalGridLines {:style axis-style}]
   [:> rvis/XAxis {:tickSizeInner 0 :tickSizeOuter 6 :style axis-style}]
   [:> rvis/YAxis {:tickSizeInner 0 :tickSizeOuter 6 :style axis-style}]
   (doall (for [[id perfil] @perfiles]
            ^{:key (str id)} [:> rvis/LineSeries {:data (:data-vis perfil) :style {:fill "none"}
                                                                :strokeWidth 1
                                                                :onNearestX (fn [e]
                                                                    (reset! nearest-xy (js->clj e)))}]))
   [:> rvis/Crosshair {:values [{:x (nearest-x nearest-xy) :y 0}] :strokeStyle "dashed" :strokeDasharray  "10,10"
                       :style {:line {:background "black" :strokeDasharray "10,10" }}}
      [:div]]
   [:> rvis/Crosshair {:values [{:x (nearest-x nearest-xy-pressed) :y 0}]
                       :style {:line {:background "black" :opacity (if @button-pressed? 1 0)}}}
      [:div]]
  ; (let [etiqueta (crear-etiqueta 300 4000)]
  ;    (swap! etiquetas conj etiqueta)
  ;    etiqueta)
  (crear-etiqueta 300 4000)
  ; [:> rvis/LabelSeries {:data [{:x 650 :y 4000 :label "Hidrógeno"}
  ;                              {:x 650 :y 4000 :label "alfa" :yOffset 18}]
  ;                       :style {:cursor "pointer"}
  ;                       ; :allowOffsetToBeReversed "false"
  ;                       :onValueClick (fn [d] (js/console.log (pr-str d)))}]
   ]])

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

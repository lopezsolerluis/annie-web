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
(defonce perfil-activo (atom "")) ;idem
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
         (map #(reduce + %) data)))  ; Suma sobre las filas, porque el archivo "fits" lo creé 'traspuesto'...¡casi hacker!

(defn crear-data-para-vis [perfil-2d]
  (mapv (fn [x y] {:x x :y y}) (range) perfil-2d))

(defn procesar-archivo [fits-file]
  (if (= fits-file :fits-no-simple)
      (js/alert (app-tr @lang :fits-no-valido))
      (let [perfil (crear-perfil fits-file)
            data-para-vis (crear-data-para-vis perfil)
            nombre (:nombre-archivo fits-file)]
         (reset! perfil-activo nombre)
         (swap! perfiles assoc nombre {:nombre nombre :data-vis data-para-vis :etiquetas {}})))
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
  (do (gevents/listen (gdom/getElement "crear-perfil-desde-fits") "click" #(.click (gdom/getElement "fits")))
      (gevents/listen (gdom/getElement "grabar-pestaña-annie-como") "click"
                  (fn [] (download-object-as-json (clj->js (get @perfiles @perfil-activo))
                                                  (str @perfil-activo ".annie"))))
      true))

(def nearest-xy (atom {}))
(def nearest-xy-0 (atom {}))
(defn nearest-x [nearest] (get @nearest "x"))
(defn nearest-y [nearest] (get @nearest "y"))

(def pos-mouse-pixels (r/atom {}))

(def button-izq-pressed? (r/atom false))
(def button-cen-pressed? (atom false))

(defn calcular-xy-etiqueta [position-in-pixels]
  (let [[x y] [(.-x position-in-pixels) (.-y position-in-pixels)]]
     [(- (:x @pos-mouse-pixels) x 110)
      (- (:y @pos-mouse-pixels) y 55)]))

(defn crear-etiqueta [id x y texto key-in]  ;; position is the 'delta' position in pixels
  ;(js/console.log  (pr-str key-in) (pr-str texto))
  (let [mouse-over (conj key-in :mouse-over?)
        pos (conj key-in :pos)]
;  [:<>
   ^{:key id}
    [:> rvis/CustomSVGSeries {:onValueMouseOver (fn [d] (swap! perfiles assoc-in mouse-over true))
                              :onValueMouseOut  (fn [d] (if-not @button-cen-pressed? (swap! perfiles assoc-in mouse-over false)))
                              :data [{:x x :y y
                                :customComponent (fn [_ position-in-pixels]
                                  (if (and @button-cen-pressed? (get-in @perfiles mouse-over))
                                    (swap! perfiles assoc-in pos (calcular-xy-etiqueta position-in-pixels)))
                                  (let [[inc-x inc-y] (get-in @perfiles pos)]
                                    (r/as-element [:g {:className "etiqueta"}
                                                      [:polyline {:points [0 (if (< inc-y 5) -10 5) 0 inc-y inc-x inc-y]
                                                                  :stroke "black" :fill "none"}]
                                                      [:text
                                                        (map-indexed (fn [i linea]
                                                                        ^{:key linea}[:tspan {:x inc-x :y (+ inc-y (* i 18))} linea])
                                                                      texto)
                                                        ; (for [i (range (count texto))]
                                                        ;   [:tspan {:x inc-x :y (+ inc-y (* i 18))} (get texto i)])
                                                        ; [:tspan {:x inc-x :y (+ inc-y 0)} "Hidrógeno"]
                                                        ; [:tspan {:x inc-x :y (+ inc-y 18)} "Alfa"]
                                                        ]])))}]}]
    ; ^{:key (str key "line")}
    ;  [:> rvis/CustomSVGSeries {:data [{:x x :y y
    ;                             :customComponent (fn []
    ;                               (let [[inc-x inc-y] @pos]
    ;                                (r/as-element [:g {:className "etiqueta"}
    ;                                                [:polyline {:points [0 (if (< inc-y 5) -10 5) 0 inc-y inc-x inc-y]
    ;                                                            :stroke "black" :fill "none"}]])))}]}]
    ))

(defn elegir-nombre [nombres-usados sufijo]
   (let [nombres-set (set nombres-usados)]
      (loop [n 1]
         (let [nombre (keyword (str sufijo n))]
           (if-not (nombres-set nombre)
                   (keyword nombre)
                   (recur (inc n)))))))

(defn colocar-etiqueta []
  (let [perfil (get @perfiles @perfil-activo)
        baricentro (mn/calcular-baricentro (:data-vis perfil) ; Tiene la forma {:x x :y y}
                                           (nearest-x nearest-xy-0) (nearest-x nearest-xy))
        nombre (elegir-nombre (keys (:etiquetas perfil)) "etiqueta-")
        texto ["Hidrógeno" "Beta"]
        etiqueta (assoc baricentro :texto texto :pos [0 18] :mouse-over? false)]
     (swap! perfiles assoc-in [@perfil-activo :etiquetas nombre] etiqueta)))

(defn mouse-pressed [e dir]
  (let [boton (.-button e)]   ; 0: izq, 1: centro, 2: derecho
    (case boton               ; el boton derecho me abre una ventana contextual (supongo que se puede quitar, pero...)
      0 (do (if (= dir :up) (colocar-etiqueta))
            (reset! nearest-xy-0 (if (= dir :down) @nearest-xy {}))
            (swap! button-izq-pressed? not))
      1 (swap! button-cen-pressed? not)
      2 )))

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
  [:> rvis/FlexibleXYPlot
   {:margin {:left 100 :right 50 :top 20} :onMouseDown (fn [e] (mouse-pressed e :down))
                                          :onMouseUp   (fn [e] (mouse-pressed e :up))
                                          :onMouseMove (fn [e] (mouse-moved e))}
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
   (doall (for [[id perfil] @perfiles]
               ^{:key (str id)} [:> rvis/LineSeries {:data (:data-vis perfil) :style {:fill "none"}
                                                     :strokeWidth 1
                                                     :onNearestX (fn [e]
                                                            (reset! nearest-xy (js->clj e)))}]))
   (doall (for [[id {:keys [x y texto]}] (:etiquetas (get @perfiles @perfil-activo))]
                (crear-etiqueta id x y texto [@perfil-activo :etiquetas id])))

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

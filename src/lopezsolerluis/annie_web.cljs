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
(defonce perfiles (atom []))  ; ¿defonce o def..?
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
         (swap! perfiles conj {:nombre nombre :data-vis data-para-vis})))
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

(def line-style {:fill "none" :strokeLinejoin "round" :strokeLinecap "round"})
(def axis-style {:line {:stroke "#333"}
                 :ticks {:stroke "#999"}
                 :text {:stroke "none"
                        :fill "#333"}})

(def button-pressed? (r/atom false))
(defn cambiar-estado-boton [e]
  (let [boton (.-button e)] ; 0: izq, 1: centro, 2: derecho
    (if (= boton 0)         ; el boton derecho me abre una ventana contextual (supongo que se puede quitar, pero...)
        (swap! button-pressed? not))))

(defn line-chart []
  (let [nearest-x (atom 0)]
    [:div.graph
    [:> rvis/FlexibleXYPlot
     {:margin {:left 100 :right 50 :top 20} :onMouseDown (fn [e] (cambiar-estado-boton e))
                                            :onMouseUp   (fn [e] (cambiar-estado-boton e))}
     [:> rvis/VerticalGridLines {:style axis-style}]
     [:> rvis/HorizontalGridLines {:style axis-style}]
     [:> rvis/XAxis {:tickSizeInner 0 :tickSizeOuter 6 :style axis-style}]
     [:> rvis/YAxis {:tickSizeInner 0 :tickSizeOuter 6 :style axis-style}]
     (doall (for [perfil @perfiles]
              ^{:key (str (:nombre perfil))} [:> rvis/LineSeries {:data (:data-vis perfil) :style {:fill "none"}
              :onNearestX (fn [e] (if @button-pressed? (reset! nearest-x (get (js->clj e) "x"))))}]))
    (if @button-pressed?
      ^{:key (str "cursor")} [:> rvis/LineSeries {:data [{:x 247 :y 4000}{:x 247 :y 9500}] :strokeStyle "dashed" :color "black"
                                                  :opacity .5}]
      )
      ]]))

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

(ns ^:figwheel-hooks lopezsolerluis.annie-web
  (:require
   [goog.dom :as gdom]
   [goog.events :as gevents]
   [reagent.core :as r :refer [atom]]
   [reagent.dom :as rdom]
   [cljsjs.react-vis :as rvis]
   [lopezsolerluis.traducciones :as trad :refer [app-tr translations]]
   [lopezsolerluis.fits :as fits]
   [lopezsolerluis.metodos-numericos :as mn]))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))
(def perfiles (atom []))
(def n-perfil (atom 0))

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
  (let [data (:data fits-file)
        cabecera (:cabecera fits-file)
        ancho (:NAXIS1 cabecera)
        alto (:NAXIS2 cabecera)]
    (map (fn [n] (/ n alto))    ; Las divide por alto (2)
         (apply map + data))))  ; Suma las columnas (1)

(defn crear-data-para-vis [perfil-2d]
  (mapv (fn [x y] {:x x :y y}) (range (count perfil-2d)) perfil-2d))

(defn procesar-archivo [fits-file]
  (if (= fits-file :fits-no-simple)
      (js/alert (app-tr @lang :fits-no-valido))
      (let [perfil (crear-perfil fits-file)
            data-para-vis (crear-data-para-vis perfil)]

      ;   ;;(js/console.log (take 5 data-para-vis))
         (swap! perfiles conj data-para-vis)
      )))

(defn input-fits-file []
  [:input {:type "file" :id "fits" :name "imagenFits" :accept "image/fits" ;; este atributo no funciona...
           :on-change (fn [this]
                        (if (not (= "" (-> this .-target .-value)))
                          (let [^js/File file (-> this .-target .-files (aget 0))]
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
(defn line-chart []
  [:div.graph
  [:> rvis/FlexibleXYPlot
   {:margin {:left 100 :right 50 :top 20}}
   [:> rvis/VerticalGridLines {:style axis-style}]
   [:> rvis/HorizontalGridLines {:style axis-style}]
   [:> rvis/XAxis {:tickSizeInner 0 :tickSizeOuter 6 :style axis-style}]
   [:> rvis/YAxis {:tickSizeInner 0 :tickSizeOuter 6 :style axis-style}]
   (doall (for [data @perfiles]
            ^{:key (str "perfil-" (swap! n-perfil inc))} [:> rvis/LineSeries {:data data :style {:fill "none"}}]))]])

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

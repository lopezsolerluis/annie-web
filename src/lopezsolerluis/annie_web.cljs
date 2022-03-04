(ns ^:figwheel-hooks lopezsolerluis.annie-web
  (:require
   [goog.dom :as gdom]
   [goog.events :as gevents]
   [reagent.core :as r :refer [atom]]
   [reagent.dom :as rdom]
   [lopezsolerluis.traducciones :as trad :refer [app-tr translations]]
   [lopezsolerluis.fits :as fits]
   [lopezsolerluis.metodos-numericos :as mn :refer [promedio columna-matriz]]))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

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
        ancho (:NAXIS1 cabecera)]
        (js/console.log ancho)
        (js/console.log (count (first data)))
    (for [i (range ancho)]
      (promedio (columna-matriz data i)))))

(defn procesar-archivo [fits-file]
  (if (= fits-file :fits-no-simple)
      (js/alert (app-tr @lang :fits-no-valido))
      (js/console.log (clj->js (vec (crear-perfil fits-file))))
      ))

(defn input-file []
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

(defn get-app-element []
  (gdom/getElement "app"))

(defn mount [el]
  (rdom/render [input-file] el))

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

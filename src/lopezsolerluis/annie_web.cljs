(ns ^:figwheel-hooks lopezsolerluis.annie-web
  (:require
   [goog.dom :as gdom]
   [reagent.core :as r :refer [atom]]
   [reagent.dom :as rdom]
   [lopezsolerluis.traducciones :as trad :refer [app-tr translations]]))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

;; Translation functions
(defn getLanguage []
  (-> (or (.-language js/navigator) (.-userLanguage js/navigator) "en")
      (subs 0 2)))
(def lang (r/atom (keyword (getLanguage))))
(def nombres (map first (:en translations)))
(defn traducir
  ([] (traducir @lang))
  ([lang]
    (doseq [nombre nombres]
      (let [el (gdom/getElement (name nombre))]
        (set! (.-innerHTML el) (app-tr lang nombre))))))
;; end of translation functions

(defn get-app-element []
  (gdom/getElement "app"))

(defn mount [el]
  (rdom/render [:div] el))

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

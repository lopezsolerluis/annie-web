(ns lopezsolerluis.save-load-file
   (:require
     [clojure.edn]))

(defn write-object [nombre objeto tipo]
  (let [data-blob (js/Blob. #js [objeto] #js {:type tipo})
             link (.createElement js/document "a")]
    (set! (.-href link) (.createObjectURL js/URL data-blob))
    (.setAttribute link "download" nombre)
    (.appendChild (.-body js/document) link)
    (.click link)
    (.removeChild (.-body js/document) link)))

(defn write-pestaña [nombre pestaña]
  (write-object (str nombre ".annie") (pr-str {nombre pestaña}) "application/text"))

(defn write-svg [nombre]
  (let [svg (first (array-seq (goog.dom/getElementsByClass "rv-xy-plot__inner")))
        canvas (goog.dom/getElement "canvas-aux")
        ctx (.getContext canvas "2d")
        data (.serializeToString (js/XMLSerializer.) svg)]
        (js/console.log (pr-str svg))
    (write-object (str nombre ".svg") data "image/svg+xml;charset=utf-8")))

  ; (let [data-blob (js/Blob. #js [(pr-str {nombre pestaña})] #js {:type "application/text"})
  ;            link (.createElement js/document "a")]
  ;   (set! (.-href link) (.createObjectURL js/URL data-blob))
  ;   (.setAttribute link "download" (str nombre ".annie"))
  ;   (.appendChild (.-body js/document) link)
  ;   (.click link)
  ;   (.removeChild (.-body js/document) link)))

(defn read-pestaña [file callback]
  (let [js-file-reader (js/FileReader.)]
    (set! (.-onload js-file-reader)
      (fn [evt]
        (let [contenido (-> evt .-target .-result)]
              (callback (clojure.edn/read-string contenido))))) ; verificar que el contenido sea un mapa válido! Al menos, parece que clojure.edn/read-string comprueba que no haya código malicioso
    (.readAsText js-file-reader file)))

;; Tomado de http://marianoguerra.org/es/posts/download-frontend-generated-data-to-a-file-with-clojurescript.html
(defn to-json [v] (.stringify js/JSON v))

(defn download-object-as-json [value export-name]
        (let [data-blob (js/Blob. #js [(to-json value)] #js {:type "application/json"})
                   link (.createElement js/document "a")]
          (set! (.-href link) (.createObjectURL js/URL data-blob))
          (.setAttribute link "download" export-name)
          (.appendChild (.-body js/document) link)
          (.click link)
          (.removeChild (.-body js/document) link)))
;; You call it:  (download-object-as-json (clj->js {:hello "world"}) "myfile.json")

(ns lopezsolerluis.save-load-file)

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

(defn write-pestaña [nombre pestaña export-name]
  (let [data-blob (js/Blob. #js [(pr-str {nombre pestaña})] #js {:type "application/text"})
             link (.createElement js/document "a")]
    (set! (.-href link) (.createObjectURL js/URL data-blob))
    (.setAttribute link "download" export-name)
    (.appendChild (.-body js/document) link)
    (.click link)
    (.removeChild (.-body js/document) link)))
;; You call it:  (write-pestaña nombre pestaña "pestaña.annie")

(defn read-pestaña [file callback]
  (let [js-file-reader (js/FileReader.)]
    (set! (.-onload js-file-reader)
      (fn [evt]
        (let [contenido (-> evt .-target .-result)]
              (callback contenido)))) ; verificar que el contenido sea un mapa válido!
    (.readAsText js-file-reader file)))

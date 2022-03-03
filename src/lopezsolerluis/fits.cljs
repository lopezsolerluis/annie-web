(ns lopezsolerluis.fits)

(defn array->string
  ([array]
    (->> array
      (map char)
      (apply str)))
  ([array start end]
    (-> array
      (.slice start end)
      (array->string))))
  ;;(apply str (map char array)))

(defn leer-cabecera [uint8array]
  (let [primera-linea (array->string uint8array 0 30)]
    (js/console.log "Primera línea: " primera-linea)
    (if (not= primera-linea "SIMPLE  =                    T")
        :fits-no-simple
        :fits-simple
    )))
  ;
  ;       segunda-linea (.slice array 80 110)]
  ;
  ;     (do
  ;       (apply str (map char primera-linea))))))

(defn read-fits-file [file callback]
  (let [js-file-reader (js/FileReader.)]
    (set! (.-onload js-file-reader)
      (fn [evt]
        (let [contenido (-> evt .-target .-result)
              uint8array (js/Uint8Array. contenido)
              cabecera (leer-cabecera uint8array)]
          (callback cabecera))))
    (.readAsArrayBuffer js-file-reader file)))

(defn read-file [file callback]
  (let [js-file-reader (js/FileReader.)]
    (set! (.-onload js-file-reader)
      (fn [evt]
        (let [result (-> evt .-target .-result)
              array (js/Uint8Array. result)]
          (callback array))))
    (.readAsArrayBuffer js-file-reader file)))

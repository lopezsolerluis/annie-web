(ns lopezsolerluis.fits)

(defn leer-cabecera [array]
  (let [primera-linea (.slice array 0 30)
        segunda-linea (.slice array 80 110)]
    (if false ;;(not= primera-linea "SIMPLE  =                    T")
      :fits-no-simple
      (do
        ;(js/console.log (apply str (map char primera-linea)))
        ;(js/console.log (apply str (map char segunda-linea)))
        (apply str (map char primera-linea))))))

(defn read-fits-file [file callback]
  (if (not= (.-type file) "image/fits")
    (callback :extensión-no-fits)
    (let [js-file-reader (js/FileReader.)]
      (set! (.-onload js-file-reader)
        (fn [evt]
          (let [resultado (-> evt .-target .-result)
                array (js/Uint8Array. resultado)
                res (leer-cabecera array)]
            (callback res))))
      (.readAsArrayBuffer js-file-reader file))))

(defn read-file [file callback]
  (let [js-file-reader (js/FileReader.)]
    (set! (.-onload js-file-reader)
      (fn [evt]
        (let [result (-> evt .-target .-result)
              array (js/Uint8Array. result)]
          (callback array))))
    (.readAsArrayBuffer js-file-reader file)))

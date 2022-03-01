(ns lopezsolerluis.fits)

(defn leer-cabecera [array]
  (let [primera-linea (.slice array 0 30)
        segunda-linea (.slice array 80 160)]
    (if true ;;(not= primera-linea "SIMPLE  =                    T")
      :fits-no-simple
      (do
        (js/console.log (apply str (map char primera-linea)))
        (js/console.log (apply str (map char segunda-linea)))))))

(defn read-fits-file [file]
  (if (not= (.-type file) "image/fits")
    :extensión-no-fits
    (let [js-file-reader (js/FileReader.)]
      (set! (.-onload js-file-reader)
        (fn [evt]
          (let [resultado (-> evt .-target .-result)
                array (js/Uint8Array. resultado)]
            (leer-cabecera array))))
      (.readAsArrayBuffer js-file-reader file))))

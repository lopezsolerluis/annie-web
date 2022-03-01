(ns lopezsolerluis.fits)

(defn read-fits-file [file]
  (if (not= (.-type file) "image/fits")
    :extensión-no-fits
    (let [js-file-reader (js/FileReader.)]
      (set! (.-onload js-file-reader)
        (fn [evt]
          (let [resultado (-> evt .-target .-result)
                array (js/Uint8Array. resultado)
                primera-linea (.slice array 0 80)
                segunda-linea (.slice array 80 160)]
            (js/console.log (apply str (map char primera-linea)))
            (js/console.log (apply str (map char segunda-linea))))))
      (.readAsArrayBuffer js-file-reader file))))

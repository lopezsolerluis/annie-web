(ns lopezsolerluis.fits)

(defn read-fits-file [file]
  (if (not= (.-type file) "image/fits")
    :extensión-no-fits
    (let [js-file-reader (js/FileReader.)]
      (set! (.-onload js-file-reader)
        (fn [evt]
            (js/console.log (-> evt .-target .-result))))
            (.readAsText js-file-reader file))))

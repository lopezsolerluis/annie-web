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

(defn read-fits-file [file]
  (if (not= (.-type file) "image/fits")
    :extensión-no-fits
    (let [js-file-reader (js/FileReader.)
          reading-result (atom nil)]
      (set! (.-onload js-file-reader)
        (fn [evt]
          (let [resultado (-> evt .-target .-result)
                array (js/Uint8Array. resultado)
                res (doall (leer-cabecera array))]
            ;(js/console.log "Luis" res)
            (reset! reading-result res)
            (js/console.log "una " @reading-result))))
            ;(js/console.log "segunda: " fits-file))))
      (.readAsArrayBuffer js-file-reader file)
      (js/console.log "otra " @reading-result)
      @reading-result)))


(defn read-file [file]
  (let [js-file-reader (js/FileReader.)
        reading-result (atom)
        done? (atom false)]
    (set! (.-onload js-file-reader)
      (fn [evt]
        (let [result (-> evt .-target .-result)
              array (js/Uint8Array. result)]
          (reset! reading-result {:content array})
          (reset! done? true)
          (js/console.log "in: " (:content @reading-result)))))
    (.readAsArrayBuffer js-file-reader file)
    ;;(while (not @done?) (js/console.log (.-readyState js-file-reader)))
    (js/console.log "out: " @reading-result)
    @reading-result))

; (defn read-file-0 [file]
;   (let [js-file-reader (js/FileReader.)]
;     (set! (.-onload js-file-reader)
;       (fn [evt]
;         (let [result (-> evt .-target .-result)
;               array (js/Uint8Array. result)]
;           {:content result}))) ; <- This is the value that 'read-file' should return
;     (.readAsArrayBuffer js-file-reader file)))
;
;
; (defn read-file-1 [file]
;   (let [js-file-reader (js/FileReader.)
;         content nil]
;     (set! (.-onload js-file-reader)
;       (fn [evt]
;         (let [result (-> evt .-target .-result)
;               array (js/Uint8Array. result)]
;           (def content {:content result}))))
;     (.readAsArrayBuffer js-file-reader file))
;     content)

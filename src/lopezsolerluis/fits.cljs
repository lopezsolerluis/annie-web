(ns lopezsolerluis.fits
  (:require
    [clojure.string :as str]
    [js.DataView :as dv]))

(defn array->string
  ([array]
    (->> array
      (map char)
      (apply str)))
  ([array start end]
    (-> array
      (.slice start end)
      (array->string))))

(defn read-value-in-header [linea]
  (let [value (str/trim linea)] ;; ver en standard si va trim o trimr
    (cond
      (= value "T") true
      (= value "F") false
      (str/includes? value "'") value ; sería lindo sacar las comillas inicial y final...
      (str/includes? value ".") (js/parseFloat (str/replace value "D" "E"))
      :else (js/parseInt value))))

(def not-supported-keys #{"" "COMMENT" "HISTORY"})
(defn leer-cabecera [contenido]
  (let [uint8array (js/Uint8Array. contenido)
        primera-linea (array->string uint8array 0 30)]
    (if-not (= primera-linea "SIMPLE  =                    T")
        :fits-no-simple
        (let [cabecera (atom {})
              end? (atom false)
              length-header (atom -1)]
          (while (not @end?)
            (swap! length-header inc) ; arranca en 0
            (doseq [i (range (* @length-header 2880) (* (inc @length-header) 2880) 80)]  ; 2880 = 36*80
              (let [linea (array->string uint8array i (+ i 80))
                    pre-key (str/trimr (subs linea 0 8))]
                (if (= pre-key "END")
                    (reset! end? true)
                    (if-not (not-supported-keys pre-key)
                        (let [key (keyword pre-key)
                              value (read-value-in-header (subs linea 9 30))]
                              ;;(js/console.log (name key) ":" value)
                              (swap! cabecera assoc key value)))))))
            (assoc @cabecera :bloques-header (inc @length-header))))))

(def funciones-bytes {8 dv/get-uint8 16 dv/get-int16 32 dv/get-int32 64 dv/get-big-int64 -32 dv/get-float32 -64 dv/get-float64})

(defn leer-data [contenido cabecera]
  (let [length-header (* 2880 (:bloques-header cabecera))
        bitpix (:BITPIX cabecera)
        naxis (:NAXIS cabecera) ;; Sabemos que son 2, pero bueh...
        ;;ejes (map (fn [n] ((keyword (str "NAXIS" n)) cabecera))   ; <- Con esta bonita expresión podría recoger n ejes...
        ;;          (range 1 (dec naxis)))                          ;  pero sabemos que sólo serán 2, no? ;)
        eje-x (inc (:NAXIS1 cabecera))  ; El inc es necesario para los 'for' de abajo...
        eje-y (inc (:NAXIS2 cabecera))  ;
        bscale (get cabecera :BSCALE 1)
        bzero (get cabecera :BZERO 0)
        view (js.DataView. contenido length-header)
        funcion (get funciones-bytes bitpix)
        step (/ (js/Math.abs bitpix) 8)]
     ;; (js/console.log bitpix ":" step ":" funcion)
     (for [y (range 0 eje-y)]
        (for [x (range 0 eje-x)]
          (let [value (funcion view (* step (+ x (* y eje-x))))]
            (+ bzero (* bscale value)))))))

(defn read-fits-file [file callback]
  (let [js-file-reader (js/FileReader.)]
    (set! (.-onload js-file-reader)
      (fn [evt]
        (let [contenido (-> evt .-target .-result)
              cabecera (leer-cabecera contenido)
              data (leer-data contenido cabecera)]
          (callback {:cabecera cabecera :data data}))))
    (.readAsArrayBuffer js-file-reader file)))

(defn read-file [file callback]
  (let [js-file-reader (js/FileReader.)]
    (set! (.-onload js-file-reader)
      (fn [evt]
        (let [result (-> evt .-target .-result)
              array (js/Uint8Array. result)]
          (callback array))))
    (.readAsArrayBuffer js-file-reader file)))

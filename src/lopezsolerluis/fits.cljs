(ns lopezsolerluis.fits
  (:require
    [clojure.string :as str]))

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

(def not-supported-keys #{"" "END" "COMMENT" "HISTORY"})
(defn leer-cabecera [uint8array]
  (let [primera-linea (array->string uint8array 0 30)
        cabecera (atom {})]
    (if-not (= primera-linea "SIMPLE  =                    T")
        :fits-no-simple
        (do
          (doseq [i (range 80 (* 36 80) 80)]
            (let [linea (array->string uint8array i (+ i 80))
                  pre-key (str/trimr (subs linea 0 8))]
              (if-not (not-supported-keys pre-key)
                  (let [key (keyword pre-key)
                        value (read-value-in-header (subs linea 9 30))]
                    (swap! cabecera assoc key value)))))
          @cabecera))))

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

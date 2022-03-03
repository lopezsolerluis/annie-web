(ns lopezsolerluis.fits
  (:require
    [clojure.string :as string :refer [trimr]]))

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

(def not-supported-keys #{"" "END" "COMMENT" "HISTORY"})

(defn leer-cabecera [uint8array]
  (let [primera-linea (array->string uint8array 0 30)
        cabecera (atom {})]
    (if-not (= primera-linea "SIMPLE  =                    T")
        :fits-no-simple
        (doseq [i (range 80 (* 36 80) 80)]
          (let [linea (array->string uint8array i (+ i 80))
                pre-key (trimr (subs linea 0 8))]
            (if-not (not-supported-keys pre-key)
                (let [key (keyword pre-key)]
                  (js/console.log (str key ))))))
    )))

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

(ns lopezsolerluis.metodos-numericos)

(defn abs [x] (js/Math.abs x))
(defn sgn [x] (js/Math.sign x))

(defn cero
  ([funcion a b]
   (cero funcion a b 1e-5))
  ([funcion a b tolerancia]
   (let [f-a (funcion a)
         f-b (funcion b)]
      (cond
        (<= (abs (- a b)) tolerancia) (/ (+ a b) 2)
        (<= (abs f-a) tolerancia) a
        (<= (abs f-b) tolerancia) b
        (= (sgn f-a) (sgn f-b)) nil
        :else (let [c (/ (- (* b f-a) (* a f-b))
                         (- f-a f-b))]
                 (if (= (sgn f-a) (sgn (funcion c)))
                     (recur funcion c b tolerancia)
                     (recur funcion a c tolerancia)))))))

(defn calcular-baricentro [perfil x0 x1]
  (let [x-min (min x0 x1)
        x-max (max x0 x1)
        potencia -2
        perfil-acotado (remove (fn [punto]
                                  (let [x (:x punto)]
                                    (or (< x x-min) (> x x-max)))) perfil)
        [suma-ponderada suma-intensidades] (reduce (fn [[suma-p suma-i] {:keys [x y]}]
                                                      (let [valor (js/Math.pow y potencia)]
                                                         [(+ suma-p (* x valor))
                                                          (+ suma-i valor)]))
                                                    [0 0] perfil-acotado)]
    ; (js/console.log (/ suma-ponderada suma-intensidades) )                                                    
    (/ suma-ponderada suma-intensidades)))

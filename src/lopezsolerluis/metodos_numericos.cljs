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

(def luis 3)

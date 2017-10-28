(ns clibtrader.talib
  (:gen-class)
  (:require [clibtrader.csvjson :as csvjson])
  )

;these functions provide technical indicators for price series
;input conventions: c is collection or close, o open l low h high v volume
;input conventions: p is period



;Test vectors
(def v0 (vec (take 300 (repeatedly #(rand-int 100)))))
(def v1 (csvjson/vecFromMap (csvjson/readCSV "XXX/v1.csv") :price))
(def v2 (csvjson/vecFromMap (csvjson/readCSV "XXX/v2.csv") :price))
(def v1s (csvjson/colFromMap (csvjson/readCSV "XXX/v1.csv") :price))
(def v2s (csvjson/colFromMap (csvjson/readCSV "XXX/v2.csv") :price))


;Simple rolling functions - data will come as nil if not perfectly formed

(defn generic_rolling [fc c p]
	(lazy-cat
		(repeat (- p 1) nil)
		(map fc (partition p 1 c))
	)
)

(defmulti rolling (fn [f c p] f))
(defmethod rolling :mean [f c p]
	(generic_rolling (fn [a] (/ (reduce + a) (float p))) c p))
(defmethod rolling :max [f c p]
	(generic_rolling (fn [a] (apply max a)) c p))
(defmethod rolling :min [f c p]
	(generic_rolling (fn [a] (apply min a)) c p))

; (defmethod rolling :mean [f c p]
; 	(generic_rolling (fn [a] (/ (reduce + a) (float (count a)))) c p)
; )

;Exponential moving average

;pandas adjust=False. In practice convergence very fast to adjust=True
(defn ema [c ^double a]
  (let [a' (- 1 a)]
    (reductions
     (fn [ave x]
       (+ (* a (double x)) (* a' (double ave))))
     (first c)
     (rest c))))

(defmulti ewm (fn [f c p] f))
(defmethod ewm :alpha [f c p] (ema c p))
(defmethod ewm :com [f c p] (ema c (/ 1 (+ p 1))))
(defmethod ewm :span [f c p] (ema c (/ 2 (+ p 1))))



(defn addv [v1 v2]
  (map + v1 v2)
  )

(defn diffv [v1 v2]
  (map - v1 v2)
  )

(defn divvsafe [v1 v2]
  (map (fn [a b] (if (zero? b) nil (/ a b))) v1 v2)
  )

(defn dvl [v]
  (conj [nil] (diffv (rest v) (drop-last v)))
  )

(defn dv [v]
  (diffv (rest v) (drop-last v))
  )

;Relative strength index - data will come as nil if not perfectly formed
(defn rsiv [v p]
  (let [d (dv v)
        u (mapv (fn [x] (if (pos? x) x 0)) d)
        d (mapv (fn [x] (if (neg? x) (- x) 0)) d)
        uede (divvsafe (ewm :span u p) (ewm :span d p))
        ]
    (mapv (fn [x] (if (zero? x) nil (- 100 (/ 1 (+ 1 x))))) uede)
    )
  )

;Relative strength index - data will come as nil if not perfectly formed
(defn rsi [v p]
  (let [d (dv v)
        u (map (fn [x] (if (pos? x) x 0)) d)
        d (map (fn [x] (if (neg? x) (- x) 0)) d)
        uede (divvsafe (ewm :span u p) (ewm :span d p))
        ]
    (map (fn [x] (if (nil? x) nil (- 100 (/ 100 (+ 1 x))))) uede)
    )
  )

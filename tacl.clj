(ns tacl.tacl
  (:gen-class)
  )
;These functions provide technical indicators for price series
;They are tested against Python Pandas.
;input conventions: c is collection or close, o open l low h high v volume
;input conventions: p is period


;Basic point arithmetic
(defn +s [v1 v2] (map + v1 v2))
(defn -s [v1 v2] (map - v1 v2))
(defn *s [v1 v2] (map * v1 v2))
(defn divssafe [v1 v2] (map #(if (zero? %2) nil (/ %1 %2)) v1 v2))


;Delta functions
(defn ds [v] (-s (rest v) (drop-last v)))
(defn dvv [v] (conj (ds v) nil)) ; append at beginning of sequence
(defn dss [v] (conj [nil] (ds v))) ; append at beginning of vector

;Filling data forward
(defn ffill [v]
  (letfn [(f [yday tday] (if (nil? tday) yday tday))
          (g [coll tday] (conj coll (f (peek coll) tday)))]
    (reduce g [(first v)] (rest v))))

(defn vec-to-map [v k] (into {} (for [r v] {(k r) r})))

;(defn align-vectors [v1 v2 key1 key2]
;  (let [v2map (vec-to-map v2 key2)]
;    (into [] (for [r v1]
;      (let [row (v2map (key1 r))]
;        (if (nil? row) {key1 (key1 r)} row))))))

(defn align-vectors [v1 v2 key1 key2]
  (let [v2map (vec-to-map v2 key2)]
    (into [] (for [r v1] (if-let [row (v2map (key1 r))] row {key1 (key1 r)})))))


;Financial returns
(defn growth [c] (divssafe (rest c) (drop-last c)))
(defn returns [c] (map #(- (double %) 1) (growth c)))
(defn vai [c] (reductions * (growth c)));value added index
(defn underwater [c] (-s c (reductions max c)))
(defn maxdrawdown [c] (apply min (underwater c)))
(defn maxdrawdownpc [c] (let [max2here (reductions max c)] (apply min (divssafe (-s c max2here) max2here))))

;Basic statistics
(defn mean [c] (/ (reduce + c) (count c))) 
(defn covariance [c1 c2]
  (let [m1 (mean c1) m2 (mean c2)]
    (/
      (reduce + (map #(* (- %1 m1) (- %2 m2)) c1 c2))
      (count c1))))
(defn variance [c] (covariance c c))
(defn stdevp [c] (Math/sqrt (variance c)))
(defn correlation [c1 c2] (/ (covariance c1 c2) (* (stdevp c1) (stdevp c2))))
(defn stdev [c] (* (Math/sqrt (/ (count c) (dec (count c)))) (stdevp c)))
(def square #(* % %))
;(defn stdev [c]
;  (let [m (mean c)]
;    (Math/sqrt
;      (/ (reduce #(+ %1 (square (- %2 m))) 0 c)
;         (dec (count c))))))
(defn sharpe [c] (/ (mean c) (stdev c)))
(defn sortino [c t]
  (let [m (- (mean c) t)
        dstd (Math/sqrt
              (/  (reduce #(+ %1 (square (min 0 (- %2 t)))) 0 c)
                  (dec (count c))))]
    (/ m dstd)))


;;;;;;;;;;;;;;;;;
;MOVING AVERAGES;
;;;;;;;;;;;;;;;;;

(defn moving-average ;Fast implementation
  "Calculates the moving average of values with the given period.
  Returns a lazy seq, works with infinite input sequences.
  Does not include initial zeros in the output."
  [values period]
  (letfn [(gen [last-sum values-old values-new]
              (if (empty? values-new)
                nil
                (let [num-out (first values-old)
                      num-in  (first values-new)
                      new-sum (+ last-sum (- num-out) num-in)]
                  (lazy-seq
                    (cons new-sum
                          (gen new-sum
                               (next values-old)
                               (next values-new)))))))]
    (if (< (count (take period values)) period)
      nil
      (map #(/ % period)
           (gen (apply + (take (dec period) values))
                (cons 0 values)
                (drop (dec period) values))))))

(defn ema [c ^double a]
  "Exponential moving average
  Matches Python Pandas adjust=False.
  In practice convergence very fast to adjust=True."
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

(defn rsi [c p]
  "Relative strength index.
  Matches talib perfectly.
  Data will come as nil if not perfectly formed.
  Note definition of alpha matching neither com nor span.
  "
  (let [d (ds c)
        u (map #(if (pos? %) % 0) d)
        d (map #(if (neg? %) (- %) 0) d)
        uede (divssafe (ewm :alpha u (/ 1. p)) (ewm :alpha d (/ 1. p)))]
    (map #(if (nil? %) nil (- 100 (/ 100 (+ 1 %)))) uede)))

(defn rsi-legacy [c p]
  "Wrong implementation, matches old Python code used for backtesting"
  (rsi c (/ (+ p 1) 2)))

;;;;;;;;;;;;;;;;

(defn ascending-minmax [c p cmp]
  "Fast implementation using Java Deque"
  (let [window (ArrayDeque. p) n (count c) out (atom []) i (atom 0)]
    (while (< @i n)
      (let [ci (nth c @i)]
        (while (and (pos? (.size window)) (cmp (first (.peekFirst window)) ci))
          (.removeFirst window)) 
        (.addFirst window [ci @i])
        (while (<= (last (.peekLast window)) (- @i p))
          (.removeLast window))
        (swap! out (fn [x] (conj x (first (.peekLast window)))))
        (swap! i inc)))
    @out)
)

;(defn ascending-maxima [c p] (ascending-minmax c p <=))
;(defn ascending-minima [c p] (ascending-minmax c p >=))

;;;;;;;;;;;;;;
;UNUSED BELOW;
;;;;;;;;;;;;;;

;Simple rolling functions - data will come as nil if not perfectly formed
;this is not fast enough for long periods
(defn generic-rolling [fc c p] (map fc (partition p 1 c)))

(defn generic_rolling_fixed_width [fc c p]
  (lazy-cat (repeat (- p 1) nil) (generic-rolling fc c p)))

;this will only be faster for (realistic) cases where there are trends
(defn max-with-trends [c p]
  (letfn [(f [sq priormax]
            (if (empty? sq)
              ()
              (let [w (first sq) lw (last w) nm (if (> lw priormax) lw (apply max w))]
                (lazy-seq (cons nm (f (next sq) nm))))))]
    (f (partition p 1 c) -1000)))


;;;;;;;;;;;;;;;;;;;
;DISPATCH FUNCTION;
;;;;;;;;;;;;;;;;;;;

(defmulti rolling (fn [f c p] f))
(defmethod rolling :slow-mean [f c p] (generic-rolling (fn [a] (/ (reduce + a) (float p))) c p))
(defmethod rolling :slow-max [f c p]  (generic-rolling (fn [a] (apply max a)) c p))
(defmethod rolling :slow-min [f c p]  (generic-rolling (fn [a] (apply min a)) c p))
(defmethod rolling :max [f c p]     (ascending-minmax c p <=))
(defmethod rolling :min [f c p]     (ascending-minmax c p >=))
(defmethod rolling :mean [f c p]    (moving-average c (float p)))


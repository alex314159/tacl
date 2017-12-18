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
(defn divssafe [v1 v2] (map (fn [a b] (if (zero? b) nil (/ a b))) v1 v2))


;Delta functions
(defn ds [v] (-s (rest v) (drop-last v)))
(defn dvv [v] (conj (ds v) nil)) ; append at beginning of sequence
(defn dss [v] (conj [nil] (ds v))) ; append at beginning of vector


;Financial returns
(defn growth [c] (divssafe (rest c) (drop-last c)))
(defn returns [c] (map #(- % 1) (growth c)))
(defn vai [c] (reductions * (growth c)));value added index
(defn underwater [c] (-s c (reductions max c)))
(defn maxdrawdown [c] (apply min (underwater c)))

;Basic statistics
(defn mean [c] (/ (reduce + c) (count c))) 
(def square #(* % %))
(defn stdev [c]
  (let [m (mean c)]
    (Math/sqrt
      (/ (reduce #(+ %1 (square (- %2 m))) 0 c)
         (dec (count c))))))
(defn sharpe [c] (/ (mean c) (stdev c)))

;Moving averages
(defn moving_average ;Fast implementation
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



;Simple rolling functions - data will come as nil if not perfectly formed
;this is not fast enough for long periods
(defn generic_rolling_fixed_width [fc c p]
  (lazy-cat
    (repeat (- p 1) nil)
    (map fc (partition p 1 c))))

(defn generic_rolling [fc c p] (map fc (partition p 1 c)))


(defn rsi [c p]
  "Relative strength index.
  Matches talib perfectly.
  Data will come as nil if not perfectly formed.
  Note definition of alpha matching neither com nor span.
  "
  (let [d (ds c)
        u (map (fn [x] (if (pos? x) x 0)) d)
        d (map (fn [x] (if (neg? x) (- x) 0)) d)
        uede (divssafe (ewm :alpha u (/ 1. p)) (ewm :alpha d (/ 1. p)))]
    (map (fn [x] (if (nil? x) nil (- 100 (/ 100 (+ 1 x))))) uede)))

(defn rsi_legacy [c p]
  "Wrong implementation, matches old Python code used for backtesting"
  (rsi c (/ (+ p 1) 2)))

(defn rsi_legacy_old [c p]
  "Wrong implementation, matches old Python code used for backtesting"
  (let [d (ds c)
        u (map (fn [x] (if (pos? x) x 0)) d)
        d (map (fn [x] (if (neg? x) (- x) 0)) d)
        uede (divssafe (ewm :span u p) (ewm :span d p))]
    (map (fn [x] (if (nil? x) nil (- 100 (/ 100 (+ 1 x))))) uede)))

;;;;;;;;;;;;;;;;

;this will only be faster for (realistic) cases where there are trends
(defn max2 [c p]
    (letfn [(f [sq priormax]
              (if (empty? sq)
                  ()
                  (let [w (first sq) lw (last w) nm (if (> lw priormax) lw (apply max w))]
                    (lazy-seq (cons nm (f (next sq) nm))))))]
    (f (partition p 1 c) -1000)))


(defn ascending_minmax [c p cmp]
  "Fast implementation using Java Deque"
  (let [window (java.util.ArrayDeque. p) n (count c) out (atom []) i (atom 0)]
    (while (< @i n)
      (let [ci (nth c @i)]
        (while
          (and
            (not (zero? (.size window)))
            (cmp (first (.peekFirst window)) ci))
          (.removeFirst window)
        ) 
        (.addFirst window [ci @i])
        (while (<= (last (.peekLast window)) (- @i p))
          (.removeLast window)
        )
        (swap! out (fn [x] (conj x (first (.peekLast window)))))
      (swap! i inc)
      )
    )
    @out
  )
)

(defn ascending_maxima [c p] (ascending_minmax c p <=))
(defn ascending_minima [c p] (ascending_minmax c p >=))

;Dispatch function
(defmulti rolling (fn [f c p] f))
(defmethod rolling :oldmean [f c p] (generic_rolling (fn [a] (/ (reduce + a) (float p))) c p))
(defmethod rolling :oldmax [f c p]  (generic_rolling (fn [a] (apply max a)) c p))
(defmethod rolling :oldmin [f c p]  (generic_rolling (fn [a] (apply min a)) c p))
(defmethod rolling :max [f c p]     (ascending_maxima c p))
(defmethod rolling :min [f c p]     (ascending_minima c p))
(defmethod rolling :mean [f c p]    (moving_average c (float p)))


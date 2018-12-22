;; Regions
;; type: rocky narrow wet
;; coordinates X,Y

;; geologic-index
;; mouth, target  0
;; Y = 0 (* 16807 X)
;; X = 0 (* 48271 Y)
;; (* (erosion-level (dec x) y) (erosion-level x (dec y))

;; erosion-level
;; (mod (+ geologic-index depth) 20183)

;; Cave : depth + target-coordinates

;; type
;; (mod erosion-level 3)
;; 0 rocky
;; 1 wet
;; 2 narrow

(defn all-coords [[x y]]
  (for [x' (range (inc x))
        y' (range (inc y))]
    [x' y']))


(defn calculate-index [[x y] target levels]
  (cond
    (= [0 0] [x y]) 0
    (= target [x y]) 0
    (zero? y) (* 16807 x)
    (zero? x) (* 48271 y)
    :else (* (levels [(dec x) y]) (levels [x (dec y)]))))

(defn calculate-level [depth index]
  (mod (+ depth index) 20183))

(defn reduce-fn [depth target state coord]
  (let [
        index (calculate-index coord target (:levels state))
        level (calculate-level depth index)
        t (mod level 3)
        ]
    (-> state
        (update :indexes #(assoc % coord index))
        (update :levels #(assoc % coord level))
        (update :types #(assoc % coord t)))))

(defn solve-1 [depth target]
  (reduce + (map val (:types
                      (reduce
                       (partial reduce-fn depth target)
                       {
                        :indexes {}
                        :levels {}
                        :types {}
                        }
                       (all-coords target)
                       )))))

#_(solve-1 510 [10 10])

(solve-1 4080 [14 785])

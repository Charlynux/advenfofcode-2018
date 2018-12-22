(ns adventofcode2018.day9)

(defn insert-at [index seq n]
  (let [[start end] (split-at index seq)]
    (into [] (concat start [n] end))))

(defn insert-marble [{current-index :current marbles :marbles :as state} n]
  (let [index (mod (+ 2 current-index) (count marbles))
        next-current (if (zero? index) (count marbles) index)]
    (-> state
        (assoc :marbles (insert-at next-current marbles n))
        (assoc :current next-current))))

#_(reduce insert-marble { :current 0 :marbles [0] } (range 1 15))

(defn remove-marble [player { marbles :marbles current-index :current :as state }]
  (let [index (mod (- current-index 7) (count marbles))
        value (nth marbles index)
        new-marbles (concat
                     (take index marbles)
                     (drop (inc index) marbles))]
    (->
     state
     (assoc :marbles new-marbles)
     (assoc :current index)
     (update-in [:players player] + value))))

#_(remove-marble 1 { :marbles [0 16  8 17  4 18  9 19  2 20 10 21  5 22 11  1 12  6 13  3 14  7 15] :players { 1 0 } :current 13 })

;; (0 1 2 3 4 5) current : 5 -> index -2 -> to-remove : 4
#_(mod -2 6)

(defn gen-players [n]
  (into {} (map (fn [k] [k 0])) (range n)))

#_(gen-players 5)

(defn play [state marble]
  (let [player (mod marble (count (keys (:players state))))]
    (if (= (mod marble 23) 0)
      (-> state
          (update-in [:players player] + marble)
          ((partial remove-marble player)))
      (insert-marble state marble))))

(defn game [nb-players nb-marbles]
  (reduce play
          { :players (gen-players nb-players) :marbles [0] :current 0 }
          (range 1 nb-marbles)))

#_(game 9 25)
#_(game 10 1618)
(def solve-1
  (comp
   (partial apply max)
   vals
   :players
   game))

#_(solve-1 9 25)
#_(solve-1 10 1618)
#_(solve-1 13 7999)

(time
 (solve-1 411 71170)) ;; 8 minutes !!!

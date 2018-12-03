(ns adventofcode2018.day3)

(defn init-suit [n]
  (vec (for [x (range n)]
         (vec (repeat n ".")))))

#_(init-suit 5)

(defn parse-line [line]
  (if-let [[_ id left top width height]
           (re-matches #"(#[0-9]*) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" line)]
    {:id id
     :left (Integer/parseInt left)
     :top (Integer/parseInt top)
     :width (Integer/parseInt width)
     :height (Integer/parseInt height)}))

#_(parse-line "#123 @ 3,2: 5x4")

(defn update-inch [value old-value]
  (if (= old-value ".")
    value
    "X"))

#_(update-inch "123" ".")
#_(update-inch "123" "2")
#_(update-inch "123" "X")

(defn claim->coords [{left :left top :top width :width height :height}]
  (for [x (range left (+ width left))
        y (range top (+ height top))]
    [x y]))

#_(claim->coords {:left 3 :top 2 :width 5 :height 4})

(defn update-suit [suit claim]
  (let [value (:id claim)
        coords (claim->coords claim)]
    (reduce
     (fn [acc coord] (update-in acc coord (partial update-inch value)))
     suit
     coords)))

#_(update-suit (init-suit 11) { :id "#" :left 3 :top 2 :width 5 :height 4})

(defn count-overlap [suit]
  (->> suit
       (mapcat #(filter #{"X"} %))
       count))

#_(count-overlap [[ "X" "X"] ["X" "."]])

(def example "#1 @ 1,3: 4x4
  #2 @ 3,1: 4x4
  #3 @ 5,5: 2x2")

(def data (slurp "src/day3.input"))

(->> data
     (clojure.string/split-lines)
     (map clojure.string/trim)
     (map parse-line)
     (reduce
      update-suit
      (init-suit 1000))
     (count-overlap)
     )

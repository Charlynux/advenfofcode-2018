(ns adventofcode2018.day3)

(defn init-suit [n]
  (vec (for [x (range n)]
         (vec (repeat n #{})))))

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

(def update-inch conj)

#_(update-inch #{} "value")

(defn claim->coords [{left :left top :top width :width height :height}]
  (for [x (range left (+ width left))
        y (range top (+ height top))]
    [x y]))

#_(claim->coords {:left 3 :top 2 :width 5 :height 4})

(defn update-suit [suit claim]
  (let [value (:id claim)
        coords (claim->coords claim)]
    (reduce
     (fn [acc coord] (update-in acc coord #(update-inch % value)))
     suit
     coords)))

#_(update-suit (init-suit 11) { :id "#" :left 3 :top 2 :width 5 :height 4})

(defn more-than-one [s] (> (count s) 1))

(defn count-overlap [suit]
  (->> suit
       (mapcat #(filter more-than-one  %))
       count))

#_(count-overlap [[#{1, 2} #{2 3}] [#{1 2} #{1}]])

(def example "#1 @ 1,3: 4x4
  #2 @ 3,1: 4x4
  #3 @ 5,5: 2x2")

(def data (slurp "src/day3.input"))

(def claims (->> data
                 (clojure.string/split-lines)
                 (map clojure.string/trim)
                 (map parse-line)
                 ))
;; part-1
(->> claims
     (reduce
      update-suit
      (init-suit 1000))
     (count-overlap)
     )

(->> claims
     (reduce
      update-suit
      (init-suit 1000))
     (mapcat identity)
     (reduce (fn [acc inch]
               (if (more-than-one inch)
                 (update acc :overlaps clojure.set/union inch)
                 (update acc :free clojure.set/union inch)))
             { :overlaps #{} :free #{}})
     ((fn [{ overlaps :overlaps free :free }]
        (clojure.set/difference free overlaps)))
     )

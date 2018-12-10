(ns adventofcode2018.day10)

(def input (slurp "src/day10-example.input"))

(def re-line #"position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>")

(defn parse-line [line]
  (let [data
        (partition 2 (map read-string (rest (re-find re-line line))))]
    (zipmap [:position :velocity] data)))
#_(parse-line "position=<-41214, -10223> velocity=< 4,  1>")

(def points
  (->> input
       (clojure.string/split-lines)
       (map parse-line)))

(defn move [points]
  (map
   (fn [point] (update point :position #(map + % (:velocity point))))
   points))

(into #{} (map :position) points)

(def set-points *1)

(doseq
    [x (range -5 12)]
  #_(flush)
  (doseq
      [y (range 2 5)]
    (if (set-points [x y])
      (print "#")
      (print ".")))
  (flush))

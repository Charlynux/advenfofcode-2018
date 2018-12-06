(ns adventofcode2018.day6)

(def data [[1, 1]
           [1, 6]
           [8, 3]
           [3, 4]
           [5, 5]
           [8, 9]])

(defn parse-line [line]
  (read-string (str "[" line "]")))

(def data (->> (slurp "src/day6.input")
               (clojure.string/split-lines)
               (map parse-line)))

(defn border? [grid point]
  (let [[min-grid max-grid] grid]
    (or
     (= (first point) (first min-grid))
     (= (second point) (second min-grid))
     (= (first point) (first max-grid))
     (= (second point) (second max-grid)))))

(defn manhattan-distance [[a b] [x y]]
  "d = |a - x| + |b - y|"
  (let [abs-sub #(Math/abs (- %1 %2))]
    (+ (abs-sub a x) (abs-sub b y))))

(defn generate-grid [[[x1 y1] [x2 y2]]]
  (for [xs (range x1 (inc x2))
        ys (range y1 (inc y2))]
    [xs ys]))

(let [xs
      (map first data)
      ys
      (map second data)
      grid [
            [(apply min xs) (apply min ys)]
            [(apply max xs) (apply max ys)]]
      grid-points (generate-grid grid)]
  (count grid-points))

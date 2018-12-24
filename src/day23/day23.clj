
(defn manhattan-distance [a b]
  "d = |xa - xb| + |ya - yb| + |za - zb|"
  (let [abs-sub #(Math/abs (- %1 %2))]
    (reduce + (map abs-sub a b))))

#_(manhattan-distance [0,0,0] [1,3,1])

#_(def sample-input (slurp "src/day23/day23-sample.input"))
(def input (slurp "src/day23/day23.input"))

(defn parse-nanobot [[x y z r]] [[x y z] r])

(defn parse-input [input]
  (->>
   (re-seq #"pos=<(\d+),(\d+),(\d+)>, r=(\d+)" input)
   (map rest)
   (map (partial map read-string))
   (map parse-nanobot)))

#_(parse-input sample-input)

(defn inrange-of-strongest [nanobots]
  (let [[pos radius] (apply max-key second nanobots)
        inrange? #(<=
                   (manhattan-distance pos (first %))
                   radius)]
    (filter
     inrange?
     nanobots)))

(defn solve-1 [input]
  (->>
   input
   (parse-input)
   #_(inrange-of-strongest)
   (count)))

(solve-1 sample-input)

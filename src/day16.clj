(ns adventofcode2018.day16)

(defn not-find-separators []
  (let [count (volatile! 0)]
    (fn [value]
      (if (clojure.string/blank? value)
        (vswap! count inc)
        (vreset! count 0))
      (< @count 3))))

(def input (slurp "src/day16.input"))

(def data (clojure.string/split-lines input))

(defn parse-register [register-line]
  (read-string (last (re-find #"^\w+: (.*)$" register-line))))

#_(parse-register "Before: [3, 2, 1, 1]")
#_(parse-register "After:  [3, 2, 2, 1]")

(defn parse-instruction [instruction-line]
  (map read-string (rest (re-find #"(\d+) (\d+) (\d+) (\d+)" instruction-line))))

(parse-instruction "9 2 1 2")

(defn parse-group [[before instruction after]]
  [
   (parse-register before)
   (parse-instruction instruction)
   (parse-register after)
   ])

(into [] (comp (take-while (not-find-separators))
               (remove clojure.string/blank?)
               (partition-all 3)
               (map parse-group)) data)

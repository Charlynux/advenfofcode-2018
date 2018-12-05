(ns adventofcode2018.day5)

(defn remove? [a b]
  (and (not= a b)
       (= (clojure.string/upper-case a) (clojure.string/upper-case b))))

#_(remove? "a" "A")
#_(remove? "B" "b")
#_(remove? "C" "C")

(def data (clojure.string/split "dabAcCaCBAcCcaDA" #""))

(def data (remove #{"\n"} (clojure.string/split (slurp "src/day5.input") #"")))

(def result (reduce
             (fn [acc value]
               (let [prev (last acc)]
                 (if (and (not (nil? prev)) (remove? prev value))
                   (vec (butlast acc))
                   (conj acc value)
                   )))
             []
             data))

(count result)

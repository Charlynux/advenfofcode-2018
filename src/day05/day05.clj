(ns adventofcode2018.day5)

(defn remove? [a b]
  (and (not= a b)
       (= (clojure.string/upper-case a) (clojure.string/upper-case b))))

#_(remove? "a" "A")
#_(remove? "B" "b")
#_(remove? "C" "C")

(def data (clojure.string/split "dabAcCaCBAcCcaDA" #""))

(def data (remove #{"\n"} (clojure.string/split (slurp "src/day5.input") #"")))

(defn reduced-polymer-count [polymer]
  (count (reduce
          (fn [acc value]
            (let [prev (first acc)]
              (if (and (not (nil? prev)) (remove? prev value))
                (rest acc)
                (cons value acc)
                )))
          []
          polymer)))
;;Part 1
(time (reduced-polymer-count data))


;; Part 2
(defn remove-type [type array]
  (let [types #{type (clojure.string/upper-case type)}]
    (remove types array)))

(def all-types (map (comp str char) (range 97 123)))

(first
 (sort-by second
          (map
           (fn [type] [type (reduced-polymer-count (remove-type type data))])
           all-types)))

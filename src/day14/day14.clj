(ns advenofcode2018.day14)

#_(defn digits [n]
    (->> (iterate #(quot % 10) n)
         (take-while pos?)
         (map #(mod % 10))
         reverse))

;; Stolen to mfikes mine is bugged
(def digits
  (merge
   (zipmap (range 10) (map vector (range 10)))
   (zipmap (map #(+ % 10) (range 9)) (map #(vector 1 %) (range 9)))))


#_(new-recipes 7 3)

(defn generate-recipes [{ elf1 :elf1 elf2 :elf2 recipes :recipes :as state}]
  (let [recip-1 (recipes elf1)
        recip-2 (recipes elf2)
        recipes (apply conj recipes (digits (+ recip-1 recip-2)))
        length (count recipes)]
    (-> state
        (assoc :elf1 (mod (+ elf1 recip-1 1) length))
        (assoc :elf2 (mod (+ elf2 recip-2 1) length))
        (assoc :recipes recipes))))

(def initial-state { :elf1 0 :elf2 1 :recipes [3 7] })

(defn solve-1 [input]
  (->>
   (iterate
    generate-recipes
    initial-state)
   (drop-while #(<= (count (:recipes %)) (+ input 10)))
   first
   :recipes
   (drop input)
   (take 10)
   (apply str)))

#_(solve-1 9)
#_(solve-1 5)
#_(solve-1 18)
#_(solve-1 2018)
(solve-1 30121)

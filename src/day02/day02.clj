(ns adventofcode2018.day2)

#_(def data ["abcdef"
             "bababc"
             "abbcde"
             "abcccd"
             "aabcdd"
             "abcdee"
             "ababab"])

(def data (clojure.string/split-lines (slurp "src/day2.input")))

(defn has-3-2 [freqs]
  (let [values (set (vals freqs))]
    [(if (values 3) 1 0) (if (values 2) 1 0)]))

(->> data
     (map frequencies)
     (map  has-3-2)
     (reduce
      (fn [[a b] [a' b']] [(+ a a') (+ b b')]))
     (apply *))

(defn string-diff [a b]
  (apply + (map #(if (= %1 %2) 0 1) a b)))

(defn string-equals [a b]
  (apply str (map #(if (= %1 %2) %1 "") a b)))

#_(string-equals "abc" "abd")
(->>
 (for [a data
       b data
       :when (and (not= a b) (= 1 (string-diff a b)))]
   [a b])
 first
 (apply string-equals))

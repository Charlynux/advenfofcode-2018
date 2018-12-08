(ns adventofcode2018.day8)

(def leaf [0 1 99 2 1 1 2])

(def root [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(def root (read-string (str "[" (slurp "src/day08.input") "]")))


(defn get-metadatas [[prev-metadata nb-children nb-metadata & datas]]
  (let [[children-metadata & last-datas]
        (if (= nb-children 0)
          (cons 0 datas)
          (nth (iterate get-metadatas (cons 0 datas)) nb-children))
        metadata (reduce + (take nb-metadata last-datas))]
    (cons
     (+ children-metadata metadata prev-metadata)
     (drop nb-metadata last-datas))))

(get-metadatas (cons 0 root))

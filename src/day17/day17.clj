(ns adventofcode2018.day17)

(def move-down (partial mapv + [1 0]))
(def move-right (partial mapv + [0 1]))
(def move-left (partial mapv + [0 -1]))

;; available?

[move-down
 [move-left move-right]


 ]

(def initial-state { :clays #{} :spreading #{}})

(nth
 (iterate spreading-water initial-state)
 5)

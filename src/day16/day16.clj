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

;; instruction : opcode A B C
;; A, B should be values
;; C always a register

(defn oppr [op]
  (fn [[opcode A B C] registers]
    (let [valueA (nth registers A)
          valueB (nth registers B)]
      (assoc registers C (op valueA valueB)))))

(defn oppi [op]
  (fn [[opcode A valueB C] registers]
    (let [valueA (nth registers A)]
      (assoc registers C (op valueA valueB)))))


(def addr (oppr +))
(def addi (oppi +))
#_(addr '(9 2 1 2) '[3, 2, 1, 1])

(def mulr (oppr *))
(def muli (oppi *))

(def banr (oppr bit-and))
(def bani (oppi bit-and))

(def borr (oppr bit-or))
(def bori (oppi bit-or))

(defn setr [[opcode A _ C] registers]
  (let [valueA (nth registers A)]
    (assoc registers C valueA)))
(defn seti [[opcode valueA _ C] registers]
  (assoc registers C valueA))

(defn testrr [test]
  (fn [[opcode A B C] registers]
    (let [valueA (nth registers A)
          valueB (nth registers B)]
      (assoc registers C (if (test valueA valueB) 1 0)))))
(defn testir [test]
  (fn [[opcode valueA B C] registers]
    (let [valueB (nth registers B)]
      (assoc registers C (if (test valueA valueB) 1 0)))))
(defn testri [test]
  (fn [[opcode A valueB C] registers]
    (let [valueA (nth registers A)]
      (assoc registers C (if (test valueA valueB) 1 0)))))

(def gtir (testir >))
(def gtri (testri >))
(def gtrr (testrr >))

(def eqir (testir =))
(def eqri (testri =))
(def eqrr (testrr =))

(def opcodes [
              addr addi
              mulr muli
              banr bani
              borr bori
              setr seti
              gtir gtri gtrr
              eqir eqri eqrr
              ])

(defn valid-ops [before instruction after]
  (into #{}
        (filter #(= after (% instruction before)) opcodes)))

(defn group->ops [[before instruction after]]
  { (first instruction) (valid-ops before instruction after)})


#_(group->ops '[[3, 2, 1, 1] (9 2 1 2) [3, 2, 2, 1]])

(def xform
  (comp (take-while (not-find-separators))
        (remove clojure.string/blank?)
        (partition-all 3)
        (map parse-group)
        (map group->ops)
        ))

(def xform-solve1 (comp xform (filter #(>= (count (first (vals %))) 3))))

;; Part 1
(transduce
 xform-solve1
 (fn ([val] val) ([acc _] (inc acc)))
 0
 data)


;; Part 2

(defn remove-op [state op]
  (into {} (map
            (fn [[key ops]]
              (if (= 1 (count ops))
                [key ops]
                [key (disj ops op)])) state)))

(defn remove-known-ops [state]
  (reduce
   remove-op
   state
   (map first
        (filter #(= (count %) 1)
                (map second state)))))

(defn merge-ops
  ([value] value)
  ([acc value]
   (merge-with clojure.set/intersection acc value)))

;; read first section to create a map number -> op
(defn data->ops [data]
  (->> (transduce
        xform
        merge-ops
        {}
        data)
       (iterate remove-known-ops)
       (drop-while (fn [state] (some #(> (count (second %)) 1) state)))
       first
       (map (fn [[k v]] [k (first v)]))
       (into {})))

(defn data->instructions [data]
  (->> data
       (drop-while (not-find-separators))
       (remove clojure.string/blank?)
       (map parse-instruction)))

(defn solve-2 [data]
  (let [ops (data->ops data)
        instructions (data->instructions data)]
    (reduce
     (fn [registers instruction]
       (let [op (get ops (first instruction))]
         (op instruction registers)))
     [0 0 0 0]
     instructions)))

(solve-2 data)

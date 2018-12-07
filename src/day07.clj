(ns adventofcode2018.day7)

(def input (slurp "src/day07.input"))

(defn parse-line [line]
  (rest (re-find #"Step (\w) must be finished before step (\w) can begin." line)))

(def data
  (->> input
       (clojure.string/split-lines)
       (map parse-line)))

(defn find-next-dependency [dependencies]
  (->> (dissoc dependencies :result)
       (filter (comp empty? val))
       (map key)
       (sort)
       (first)))

(defn remove-dependency [dependencies dependency]
  (reduce
   (fn [acc k] (update acc k disj dependency))
   dependencies
   (remove #{:result} (keys dependencies))))

#_(remove-dependency { "A" #{} "B" #{"A" "F"} :result "UZYE" } "A")

(defn apply-dependency [dependencies dependency]
  (-> dependencies
      (dissoc dependency)
      (update :result str dependency)
      (remove-dependency dependency)))

(defn iterate-step [dependencies]
  (apply-dependency dependencies (find-next-dependency dependencies)))

(->> data
     (reduce
      (fn [acc [dep key]]
        (->
         acc
         (update key (fnil conj #{}) dep)
         (update dep (fn [value] (if (nil? value) #{} value)))))
      { :result "" })
     (iterate iterate-step)
     (drop-while (fn [deps] (> (count (keys deps)) 1)))
     first
     :result)


;; Part 2

;;; Data Structure
;;; { :result "" :deps {} :workers {}}

;; find-next-dependencies
;; (take nb-workers)
;; append-to-workers
;; workers-work
;; find-work-done (sort by alpha)
;; remove-workers + apply-dependency + append to result

;; iterate -> take-while (not-empty? deps workers)

(def deps-keys (into #{} (flatten data)))
(def deps (reduce
           #(update %1 (second %2) conj (first %2))
           (zipmap deps-keys (repeat #{}))
           data))

(def initial-state
  { :result "" :deps deps :workers {}})

(defn find-next-dependencies [deps]
  (->> deps
       (filter (comp empty? val))
       (map key)
       sort))

(defn seconds [letter]
  (+ 60 (- (int (.charAt letter 0)) 64)))

#_(seconds "Z")

(defn map-vals [m f]
  (into {} (map (fn [[k v]] [k (f v)])) m))

(defn remove-dependencies [deps workers]
  (reduce
   #(remove-dependency %1 %2)
   deps
   workers))

(defn part2-clean-step [state]
  (let [done-workers (sort (map key (filter (comp zero? val) (:workers state))))]
    (->
     state
     (update :workers #(apply dissoc % done-workers))
     (update :deps  remove-dependencies done-workers)
     (update :result #(apply str % done-workers)))
    ))

(defn part2-work-step [state]
  (let [nb-workers (- 5 (count (keys (:workers state))))
        next-dependencies (take nb-workers (find-next-dependencies (:deps state)))
        next-workers (into {} (map (fn [l] [l (seconds l)])) next-dependencies)]
    (->
     state
     (update :workers merge next-workers)
     (update :deps #(apply dissoc % next-dependencies))
     (update :workers map-vals dec))))

#_(part2-work-step initial-state)
#_(part2-clean-step initial-state)

(->> initial-state
     (iterate (comp part2-clean-step part2-work-step))
     (take-while (fn [{workers :workers deps :deps}]
                   (or (not (empty? workers))
                       (not (empty? deps)))))
     count)

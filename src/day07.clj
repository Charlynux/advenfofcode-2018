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

(remove-dependency { "A" #{} "B" #{"A" "F"} :result "UZYE" } "A")

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

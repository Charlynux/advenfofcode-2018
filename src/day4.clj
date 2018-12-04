(ns adventofcode2018.day4
  (:require [clj-time.coerce :as c]))

(defn line->record [line]
  (if (clojure.string/ends-with? line "shift")
    (let [guard-id (second (re-find #".* Guard #(\d+) begins shift" line))]
      { :id (read-string guard-id) :type "shift" })
    (let [[day minutes action] (rest (re-find #"\[(.*) .*:(\d\d)\] (.*)" line))]
      { :day day :minutes (Integer/parseInt minutes) :type action})))



#_(line->record "[1518-11-01 00:00] Guard #10 begins shift")
#_(line->record "[1518-11-01 00:05] falls asleep")


#_(def input (slurp "src/day4-sample.input"))
(def input (slurp "src/day4-sample-random.input"))

(defn group-by-guard [acc record]
  (if (= (:type record) "shift")
    (assoc acc :current (:id record))
    (update-in acc [:moves (:current acc)] (fnil #(conj % record) []))))

(->>
 input
 (clojure.string/split-lines)
 (map line->record)
 (reduce
  group-by-guard
  {:moves {} :current nil})
 :moves)

(def data *1)


(defn calculate-range [day]
  (->>
   (conj day { :minutes 60 :type "wake"})
   (sort-by :minutes)
   (map :minutes)
   (partition 2)
   (mapcat #(apply range %))))

(defn moves->ranges [moves]
  (->> moves
       (group-by :day)
       vals
       (map calculate-range)))

(def guards-moves (into {} (map (fn [[k moves]] [k (moves->ranges moves)])) data))

(def max-guard
  (apply
   max-key
   (fn [gm] (reduce + (map count (val gm))))
   guards-moves))



(->> (val max-guard)
     (map frequencies)
     (apply merge-with +)
     (sort-by val)
     last)

(* 58 3229)
(key max-guard)

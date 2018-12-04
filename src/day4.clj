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
(def input (slurp "src/day4.input"))

(defn group-by-guard [records]
  (:records
   (reduce
    (fn [acc record]
      (if (= (:type record) "shift")
        (assoc acc :current (:id record))
        (update-in acc [:moves (:current acc)] (fnil #(conj % record) []))))
    {:moves {} :current nil}
    records)))


(defn day-moves->ranges [day-moves]
  (->>
   (partition 2 day-moves)
   (mapcat (fn [[sleep wake]]
             (let [start (:minutes sleep)
                   end (:minutes wake)]
               (range start end))))))

(defn moves->ranges [moves]
  (->> moves
       (group-by :day)
       vals
       (map day-move->ranges)))

(def guards-moves (->>
                   input
                   (clojure.string/split-lines)
                   (sort)
                   (map line->record)
                   group-by-guard
                   (into {} (map (fn [[k moves]] [k (moves->ranges moves)])))))

(defn max-asleep [guard-moves]
  (->> guard-moves
       (map frequencies)
       (apply merge-with +)
       (sort-by val)
       last))

;; Part 1
(let [max-guard (apply max-key (fn [gm] (reduce + (map count (val gm)))) guards-moves)
      asleep-minute (max-asleep (val max-guard))]
  (* (key max-guard) (first asleep-minute)))


;; Part 2
(->> guards-moves
     (map (fn [[id moves]] [id (max-asleep moves)]))
     (sort-by (comp val second))
     last
     ((juxt first (comp first second)))
     (apply *))

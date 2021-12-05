(ns day4.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> (slurp (io/reader (io/resource "day4-input.txt")))
      (str/split #"\n\n")))

(def bingo-input-per-round
  (-> (first input)
      (str/split #",")))

(def bingo-tables
  (map (fn [value]
         (let [str-table-rows (str/split value #"\n")
               table-rows (map #(remove empty? (str/split % #" ")) str-table-rows)
               column-rows (->> (range 5)
                                (map (fn [i] (map #(nth % i) table-rows))))]
           {:rows table-rows
            :columns column-rows})) (rest input)))

(defn- elements-into-set? [{:keys [rows columns] :as bingo-table} marked-numbers-set]
  (or (some #(every? marked-numbers-set %) rows)
      (some #(every? marked-numbers-set %) columns)))

(defn- first-winner
  [bingo-tables input marked-numbers]
  (let [round-number (first input)
        marked-numbers (conj marked-numbers round-number)]
    (if-let [winner (first (filter #(elements-into-set? % marked-numbers) bingo-tables))]
      {:table-winner winner
       :marked-numbers marked-numbers
       :last-round-number round-number}
      (recur bingo-tables (rest input) marked-numbers))))

(defn- last-winner
  [bingo-tables input marked-numbers]
  (let [round-number (first input)
        marked-numbers (conj marked-numbers round-number)]
    (if-let [winners (filter #(elements-into-set? % marked-numbers) bingo-tables)]
      (let [new-bingo-tables (remove (fn [e] (some #(= % e) winners)) bingo-tables)]
        (if (and (= 1 (count winners))
                 (empty? new-bingo-tables))
          {:table-winner (first winners)
           :marked-numbers marked-numbers
           :last-round-number round-number}
          (recur new-bingo-tables (rest input) marked-numbers)))
      (recur bingo-tables (rest input) marked-numbers))))

(defn- solution [winner]
  (let [unmarked-numbers (as-> (concat (get-in winner [:table-winner :rows])
                                       (get-in winner [:table-winner :columns])) $
                           (flatten $)
                           (set $)
                           (remove (:marked-numbers winner) $)
                           (map #(Integer/parseInt %) $)
                           (apply + $))
        last-round-number (Integer/parseInt (:last-round-number winner))]
    (* unmarked-numbers last-round-number)))

(defn part1 []
  (solution (first-winner bingo-tables bingo-input-per-round #{})))

(defn part2 []
  (solution (last-winner bingo-tables bingo-input-per-round #{})))

(defn -main
  [& args]
  (print "Part1: " (time (part1)) "\n"
         "Part2: " (time (part2))))

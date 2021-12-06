(ns day6.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (slurp (io/reader (io/resource "day6-input.txt"))))

(def input->numbers
  (as-> (first (str/split input #"\n")) $
      (str/split $ #",")
      (map #(Integer/parseInt %) $)))

;; Part1 slow
(defn part-1-slow-version [lanternfish days]
  (if (= days 0)
    (count lanternfish)
    (let [new-fish (mapcat #(if (= 0 %) [6 8] [(dec %)]) lanternfish)]
      (recur new-fish (dec days)))))

;; Better solution
(def evolve-fish
  (memoize (fn [fish]
             (if (= 0 fish)
               [6 8]
               [(dec fish)]))))

(def count-evolved-fish
  (memoize (fn [fish days]
             (if (= 0 days)
               1
               (reduce + (map #(count-evolved-fish % (dec days))
                              (evolve-fish fish)))))))

(defn better-solution [lanternfish days]
  (let [grouped-fish (frequencies lanternfish)]
    (reduce + (map (fn [[fish how-many]]
                     (* how-many (count-evolved-fish fish days)))
                   grouped-fish))))

(defn -main
  [& args]
  (print "Part1: " (time (better-solution input->numbers 80)) "\n"
         "Part2: " (time (better-solution input->numbers 256))))

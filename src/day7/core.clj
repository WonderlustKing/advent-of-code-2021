(ns day7.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (slurp (io/reader (io/resource "day7-input.txt"))))

(def input->numbers
  (as-> (first (str/split input #"\n")) $
      (str/split $ #",")
      (map #(Integer/parseInt %) $)))

;;
;; PART 1
;;
(defn part1 []
  (let [sorted+grouped-crabs-positions (sort (frequencies input->numbers))
        crabs-positions (map first sorted+grouped-crabs-positions)]
    (->> (map (fn [i]
                (reduce + (map (fn [[num multi]]
                                 (* (Math/abs (- i num)) multi))
                               sorted+grouped-crabs-positions)))
              crabs-positions)
         (apply min))))

;;
;; PART 2
;; cannot understand why it doesn't produce the right result
;;

(defn part2 []
  (let [sorted+grouped-crabs-positions (sort (frequencies input->numbers))
        crabs-positions (map first sorted+grouped-crabs-positions)]
    (->> (map (fn [i]
                (reduce + (map (fn [[num multi]]
                                 (let [diff (Math/abs (- i num))]
                                   (* (reduce + (range (inc diff))) multi)))
                               sorted+grouped-crabs-positions)))
              crabs-positions)
         (apply min))))

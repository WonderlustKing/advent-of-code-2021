(ns day1.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(def input
  (map #(Integer/parseInt %)
       (line-seq (io/reader (io/resource "day1-input.txt")))))

(defn solution-puzzle1 [input]
  (count (filter (fn [[a b]] (> b a)) (partition 2 1 input))))

(defn solution-puzzle2 [input]
  (-> (map #(apply + %) (partition 3 1 input))
      solution-puzzle1))

(defn -main
  [& args]
  (print "Part1: " (time (solution-puzzle1 input)) "\n"
         "Part2: " (time (solution-puzzle2 input))))

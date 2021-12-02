(ns day2.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (line-seq (io/reader (io/resource "day2-input.txt"))))

;;------
;; PART1
;;------
(defn- horizontal+depth-totals-p1
  [input]
  (reduce (fn [[horizontal depth] line]
            (let [[action value] (str/split line #" ")
                  int-value (Integer/parseInt value)]
              (cond
                (= "forward" action) [(+ horizontal int-value) depth]
                (= "down" action) [horizontal (+ depth int-value)]
                (= "up" action) [horizontal (- depth int-value)])))
          [0 0]
          input))

(defn part1 []
  (let [[horizontal-sum depth-sum] (horizontal+depth-totals-p1 input)]
    (* horizontal-sum depth-sum)))

;;-------
;; PART-2
;;-------
(defn- horizontal+depth-totals-p2
  [input]
  (reduce (fn [[horizontal depth aim] line]
            (let [[action value] (str/split line #" ")
                  int-value (Integer/parseInt value)]
              (cond
                (= "forward" action) [(+ horizontal int-value) (+ depth (* aim int-value)) aim]
                (= "down" action) [horizontal depth (+ aim int-value)]
                (= "up" action) [horizontal depth (- aim int-value)])))
          [0 0 0]
          input))

(defn part2 []
  (let [[horizontal-sum depth-sum] (horizontal+depth-totals-p2 input)]
    (* horizontal-sum depth-sum)))

;; main
(defn -main
  [& args]
  (print "Part1: " (time (part1)) "\n"
         "Part2: " (time (part2))))

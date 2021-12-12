(ns day9.core
  (:require [clojure.java.io :as io]))

(def input
  (line-seq (io/reader (io/resource "day9-input.txt"))))

(defn- location-input->number [row idx]
  (Integer/parseInt (str (nth row idx "10"))))

(defn locations [row next-row previous-row idx]
  (concat
   [(location-input->number row idx)
    (location-input->number row (dec idx))
    (location-input->number row (inc idx))]
   (when next-row
     [(location-input->number next-row idx)])
   (when previous-row
     [(location-input->number previous-row idx)])))

(defn part1 [input]
  (->> (map-indexed (fn [idx row]
                     (let [next-row (nth input (inc idx) nil)
                           previous-row (nth input (dec idx) nil)]
                       (for [i (range (count row))
                             :let [neighbors (locations row next-row previous-row i)
                                   to-check (first neighbors)]
                             :when (and (not= 9 to-check)
                                        (= to-check (apply min neighbors)))]
                         to-check)))
                   input)
      flatten
      (map inc)
      (reduce +)))



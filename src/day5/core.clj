(ns day5.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (line-seq (io/reader (io/resource "day5-input.txt"))))

(defn- format-input [input]
  (->> (map #(str/split % #" -> ") input)
       (map (fn [[a b]]
              (let [[x1 y1] (str/split a #",")
                    [x2 y2] (str/split b #",")]
                {:x1 (Integer/parseInt x1)
                 :y1 (Integer/parseInt y1)
                 :x2 (Integer/parseInt x2)
                 :y2 (Integer/parseInt y2)})))))

(defn- same-x-or-y [input]
  (filter #(or (= (:x1 %) (:x2 %))
               (= (:y1 %) (:y2 %))) input))


(defn- rangex
  "Return range from start to end, works with diagonal lines.
   Adapted from zelark."
  [start end]
  (cond
    (< start end) (range start (inc end))
    (< end start) (range start (dec end) -1)
    :else (repeat start)))

(defn- get-points [{:keys [x1 y1 x2 y2]}]
  (map vector (rangex x1 x2) (rangex y1 y2)))

(defn- solution [input]
  (->> input
       (mapcat get-points)
       frequencies
       vals
       (filter #(> % 1))
       count))

(defn part1 [input]
  (solution (same-x-or-y (format-input input))))

(defn part2 [input]
  (solution (format-input input)))

(defn -main
  [& args]
  (print "Part1: " (time (part1 input)) "\n"
         "Part2: " (time (part2 input))))

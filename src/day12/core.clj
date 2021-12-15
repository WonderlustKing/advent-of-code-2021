(ns day12.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> (line-seq (io/reader (io/resource "day12-input.txt")))
       (map #(str/split % #"-"))))

(defn- small-cave? [cave]
  (= cave (str/lower-case cave)))

(defn- unique-caves [input]
  (set (flatten input)))

(defn- group-cave-moves [input]
  (map (fn [cave]
         (assoc {}
                cave
                (->> (filter (fn [pairs] (some #(= % cave) pairs)) input)
                     (map (fn [val] (remove #(or (= cave %)
                                                 (= "start" %)) val)))
                     flatten)))
       (unique-caves input)))

(defn- find-path [current-path grouped-cave-moves limit]
  (let [cave (last current-path)
        cave-connections (-> (filter #(= cave (first (keys %))) grouped-cave-moves) first)
        small-caves (filter small-cave? current-path)
        visit-counts (-> (frequencies small-caves) (dissoc "start") vals)
        visited (if (some #{limit} visit-counts) (set small-caves) #{"started"})
        allowed (remove visited (get cave-connections cave))]
    (cond
      (= "end" cave) [current-path]
      :else (mapcat #(find-path (conj current-path %) grouped-cave-moves limit) allowed))))

(defn part1 []
  (count (find-path ["start"] (group-cave-moves input) 1)))

(defn part2 []
  (count (find-path ["start"] (group-cave-moves input) 2)))

(defn -main
  [& args]
  (print "Part1: " (time (part1)) "\n"
         "Part2: " (time (part2))))

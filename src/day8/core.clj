(ns day8.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (line-seq (io/reader (io/resource "day8-input.txt"))))

;;
;; part 1
;;
(def input->4-digit-output-value
  (->> (map #(last (str/split % #" \| ")) input)
       (map #(str/split % #" "))))

(defn part1 []
  (->> (map #(group-by count %) input->4-digit-output-value)
       (map #(select-keys % [2 3 4 7]))
       (remove empty?)
       (map vals)
       flatten
       count))

;;
;; part 2
;;

(def part2-input
  (->> (map #(str/split % #" \| ") input)
       (map (fn [line] (map #(str/split % #" ") line)))))

(def seven-segment-display
  [{0 {:seg-num 6, :seg-vals [\a \a \c \e \f \g]}}
   {1 {:seg-num 2}}
   {2 {:seg-num 5, :seg-vals [\a \c \d \e \g]}}
   {3 {:seg-num 5, :seg-vals [\a \c \d \f \g]}}
   {4 {:seg-num 4}}
   {5 {:seg-num 5, :seg-vals [\a \b \d \f \g]}}
   {6 {:seg-num 6, :seg-vals [\a \b \d \e \f \g]}}
   {7 {:seg-num 3}}
   {8 {:seg-num 7}}
   {9 {:seg-num 6, :seg-vals [\a \b \c \d \f \g]}}])

(defn part2 []
  ;; to be continue
  )

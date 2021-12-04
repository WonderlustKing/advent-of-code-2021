(ns day3.core
  (:require [clojure.java.io :as io]))

(def input
  (line-seq (io/reader (io/resource "day3-input.txt"))))

;;
;; PART 1
;;

(defn- group-bits-numbers [input]
  (sort
   (reduce (fn [acc line]
             (apply (partial merge-with conj acc)
                    (map-indexed hash-map line)))
           {0 [], 1 [], 2 [], 3 [], 4 [], 5 [],
            6 [], 7 [], 8 [], 9 [], 10 [], 11 []}
           input)))

(defn- sorted-frequent [grouped-bits]
  (map #(vec [(first %)
              (sort-by val (frequencies (second %)))])
       grouped-bits))

(defn- gamma-rate-number [grouped-bits]
  (-> (apply str (map (fn [[_ [_ value]]]
                        (first value))
                      (sorted-frequent grouped-bits)))
      (Integer/parseInt 2)))

(defn- epsilon-rate-number [grouped-bits]
  (-> (apply str (map (fn [[_ [value]]]
                        (first value))
                      (sorted-frequent grouped-bits)))
      (Integer/parseInt 2)))

(defn part1 []
  (let [grouped-bits (group-bits-numbers input)
        gamma-rate (gamma-rate-number grouped-bits)
        epsilon-rate (epsilon-rate-number grouped-bits)]
    (* gamma-rate epsilon-rate)))

;;
;; PART 2
;;

(defn- first-position-most-frequent-bit [grouped-bits]
  (let [[_ [_ [most-frequent-bit]]] (first (sorted-frequent grouped-bits))]
    most-frequent-bit))

(defn- life-support-rating [input indx most-frequent?]
  (if (= 1 (count input))
    (Integer/parseInt (first input) 2)
    (let [input-without-indx-bit (map #(.substring % indx) input)
          frequent-bits-map (frequencies (map first input-without-indx-bit))
          bit-operator (if most-frequent? > <)
          frequent-bit (if (= (get frequent-bits-map \0) (get frequent-bits-map \1))
                              (if most-frequent? \1 \0)
                              (-> (sort-by val bit-operator frequent-bits-map) first first))
          new-input (filter #(= frequent-bit (nth % indx)) input)]    
      (recur new-input (inc indx) most-frequent?))))

(defn- oxugen-generator-value [input indx]
  (life-support-rating input indx true))

(defn- co2-scrubber-value [input indx]
  (life-support-rating input indx false))

(defn part2 []
  (let [grouped-bits (group-bits-numbers input)
        most-frequent-bit (first-position-most-frequent-bit grouped-bits)
        less-frequent-bit (if (= \1 most-frequent-bit) \0 \1)
        oxugen-init-input (filter #(= most-frequent-bit (first %)) input)
        co2-init-input (filter #(= less-frequent-bit (first %)) input)
        oxugen-value (oxugen-generator-value oxugen-init-input 1)
        co2-value (co2-scrubber-value co2-init-input 1)]
    (* oxugen-value co2-value)))

;; main
(defn -main
  [& args]
  (print "Part1: " (time (part1)) "\n"
         "Part2: " (time (part2))))

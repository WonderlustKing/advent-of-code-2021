(ns day10.core
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def input
  (line-seq (io/reader (io/resource "day10-input.txt"))))

(def opening-chars #{\[ \{ \( \<})

(def pair-chars [{\[ \]}
                 {\{ \}}
                 {\( \)}
                 {\< \>}])

(def point {\) 3
            \] 57
            \} 1197
            \> 25137})

(defn- opening-char?
  [char-to-check]
  (contains? opening-chars char-to-check))

(defn- right-closing-char?
  [char1 char2]
  (-> (filter #(get % char1) pair-chars)
      first
      vals
      first
      (= char2)))

(defn error-fn [line result]
  (when (empty? line) nil)
  (let [char-to-check (first line)
        list-to-check (rest line)]
    (cond
      (right-closing-char? (last result) char-to-check) (error-fn list-to-check
                                                                  (vec (butlast result)))
      (opening-char? char-to-check) (error-fn list-to-check (conj result char-to-check))
      :else char-to-check)))

(defn part1 [input]
  (->> (map #(error-fn % []) input)
       (map #(get point %))
       (reduce +)))



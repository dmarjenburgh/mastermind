(ns mastermind.strategy
  (:require [clojure-csv.core :refer [parse-csv]]))

(def all-codes
  (let [letters [\A \B \C \D \E \F]]
    (vec
      (for [n1 letters n2 letters n3 letters n4 letters]
        (str n1 n2 n3 n4)))))

(def all-scores
  (vec
    (for [black (range 5) white (range (- 5 black))]
      [black white])))

(def initial-distribution (vec (repeat 1296 (float 1/1296))))

(defn normalise [dist]
  (let [total (float (reduce + dist))]
    (mapv #(/ % total) dist)))

(defn- count-position-matches [guess secret]
  (count (filter (partial apply =) (map vector guess secret))))

(defn count-value-matches [guess secret]
  (letfn [(count-occ [x coll]
            (count (filter (partial = x) coll)))]
    (reduce + (map (fn [x]
                     (min (count-occ x guess)
                          (count-occ x secret))) (set guess)))))

(defn score
  [guess secret]
  (let [correct (count-position-matches guess secret)
        matches (count-value-matches guess secret)]
    [correct (- matches correct)]))

(defn- ^:testable compatible? [new-guess [guess hint]]
  (= (score guess new-guess) hint))

(defn  consistent-with-history? [new-guess state]
  (every? (partial compatible? new-guess) state))

(defn code->index [code]
  (reduce #(+ (* 6 %) (- (long %2) 65)) 0 code))

(defn index->code [index]
  (nth all-codes index))

(defn score->index [[black white]]
  (+ white (/ (* (- 11 black) black) 2)))

(defn index->score [index]
  (nth all-scores index))

(def lookup
  (mapv (fn [row] (mapv #(Long. %) row)) (parse-csv (slurp "resources/lookup.csv"))))

(defn lookup-indices [guess]
  (nth lookup (code->index guess)))


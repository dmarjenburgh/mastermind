(ns mastermind.benchmarks
  (:require [mastermind.guesser :as g]
            [mastermind.strategy :as s]
            [criterium.core :refer :all]))

(defn bench-max-entropy-guess []
  (let [dist s/initial-distribution]
    (bench (g/max-entropy-guess dist))))

(defn bench-score-dist []
  (let [dist s/initial-distribution]
    (quick-bench (#'g/score-dist dist (rand-nth s/all-codes)))))
;WARNING: Final GC required 4.982027887162528 % of runtime
;Evaluation count : 3900 in 60 samples of 65 calls.
;             Execution time mean : 15.536131 ms
;    Execution time std-deviation : 381.764192 µs
;   Execution time lower quantile : 14.961737 ms ( 2.5%)
;   Execution time upper quantile : 16.511911 ms (97.5%)
;                   Overhead used : 1.885280 ns
;
;Found 4 outliers in 60 samples (6.6667 %)
;	low-severe	 3 (5.0000 %)
;	low-mild	 1 (1.6667 %)
; Variance from outliers : 12.5666 % Variance is moderately inflated by outliers

(defn bench-entropy []
  (let [sd (partial #'g/score-dist s/initial-distribution)]
    (bench (for [guess s/all-codes]
             (#'g/entropy (sd guess))))))
;Evaluation count : 2180656380 in 60 samples of 36344273 calls.
;             Execution time mean : 26.150628 ns
;    Execution time std-deviation : 0.454497 ns
;   Execution time lower quantile : 25.417886 ns ( 2.5%)
;   Execution time upper quantile : 27.253628 ns (97.5%)
;                   Overhead used : 1.876023 ns
;
;Found 3 outliers in 60 samples (5.0000 %)
;	low-severe	 3 (5.0000 %)
; Variance from outliers : 6.2835 % Variance is slightly inflated by outliers

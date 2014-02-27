(ns mastermind.benchmarks
  (:require [mastermind.guesser :as g]
            [criterium.core :refer :all]))

(defn bench-normalise []
  (let [dist (vec (repeatedly 1296 (partial rand-int 10)))]
    (dotimes [_ 10]
      (time
        (dotimes [_ 1000]
          (g/normalise dist))))))

(defn bench-max-entropy-guess []
  (let [dist g/p-dist]
    (bench (max-entropy-guess dist))))

(ns mastermind.console
  (:require [mastermind.game :refer [Console]])
  (:import [java.util Scanner]))

(defn- prompt [message]
  (println message)
  (read-line))

(defn- enter-position-matches []
  (prompt "How many matches are in the correct position?"))

(defn- enter-value-matches []
  (prompt "How many matches are in the wrong position?"))


(defrecord console []
  Console
  (read-score [this guess]
    (println "\nI guess it's" guess)
    (let [pm (Long. (enter-position-matches))
          vm (if (not= pm 4) (Long. (enter-value-matches)) 0)]
      [pm vm]))
  (announce-winning-code [this code] (println "The code was:" code))
  (announce-tries [this tries] (println "It took" tries "tries."))
  (announce-bad-scoring [this] (println "No guess exists consistent with the given scores."
                                        "Your scoring was sub-optimal."))
  (announce-game-over [this] (println "Game Over.")))

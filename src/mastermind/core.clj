(ns mastermind.core
  (:require [mastermind.guesser :as g]
            [mastermind.console :as c]
            [mastermind.game :refer [play]]))

(defn- pick-guesser []
  (println "\nChoose a guessing strategy for the computer")
  (println "1. Random (dumb, avg 1296 guesses)")
  (println "2. With memory (ok, avg 5.7 guesses)")
  (println "3. Bayesian maximum entropy (smart, avg 4.415 guesses)")
  (println "\n[1-3]: ")
  (Long. (read-line)))

(defn -main [& args]
  (let [guesser (case (pick-guesser)
                  1 (g/->random-guesser)
                  2 (g/->r-guesser (atom {}))
                  3 (g/->max-ent-guesser (atom {})))
        console (c/->console)]
    (play guesser console)))

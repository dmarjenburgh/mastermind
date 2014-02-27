(ns mastermind.game)

(defprotocol Console
  (read-score [this guess])
  (announce-winning-code [this code])
  (announce-tries [this tries])
  (announce-bad-scoring [this])
  (announce-game-over [this]))

(defprotocol Guesser
  (init-memory [this])
  (make-guess [this])
  (add-hint [this guess hint]))

(defn- win [guess c tries]
  (announce-winning-code c guess)
  (announce-tries c tries)
  (announce-game-over c))

(defn- fail [c]
  (announce-bad-scoring c)
  (announce-game-over c))

(defn play [g c]
  (init-memory g)
  (loop [tries 1]
    (if-let [guess (make-guess g)]
      (let [[black white :as score] (read-score c guess)]
        (if (= black 4)
          (win guess c tries)
          (do
            (add-hint g guess score)
            (recur (inc tries)))))
      (fail c))))

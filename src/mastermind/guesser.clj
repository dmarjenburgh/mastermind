(ns mastermind.guesser
  (:require [clojure.set :refer [intersection]]
            [mastermind.game :refer [Guesser]]
            [mastermind.strategy :refer :all]
            [clojure.string :as str]
            [clojure-csv.core :refer :all]
            [slingshot.slingshot :refer [throw+ try+]]))

(defn random-guess []
  (let [random-char (partial rand-nth [\A \B \C \D \E \F])]
    (apply str (repeatedly 4 random-char))))

(defn remembering-guess [history]
  (let [find-comb (fn [guess] (when (consistent-with-history? guess history)
                                guess))
        letters [\A \B \C \D \E \F]
        combination (some find-comb all-codes)]
    combination))

(defn- ^:testable bayesian-update [dist guess hint]
  (letfn [(likelihood [code] (if (= (score guess code) hint) 1 0))
          (posterior [v index prob] (assoc! v index (if (zero? prob) 0 (likelihood (index->code index)))))]
    (normalise (persistent! (reduce-kv posterior (transient []) dist)))))

(defn- ^:testable entropy [dist]
  (- (reduce + (mapv (fn [p] (if (zero? p) 0 (* p (Math/log p)))) dist))))

(defn- ^:testable score-dist [dist guess]
  (let [indices (lookup-indices guess)]
    (persistent!
      (reduce-kv
        (fn [v index prob]
          (let [score-index (nth indices index)]
            (assoc! v score-index (+ (v score-index) prob))))
        (transient (vec (repeat 15 0))) dist))))

(defn- certain-answer? [dist]
  (if (some (partial == 1) dist)
    (some identity (map-indexed
                     (fn [index p]
                       (when (== p 1)
                         (index->code index))) dist))))

(defn max-entropy-guess [dist]
  (if-let [answer (certain-answer? dist)]
    answer
    (let [sd (partial score-dist dist)]
      (loop [m-ent 0 [code & r] all-codes index 0 guess nil]
        (if code
          (let [s (entropy (sd code))]
            (if (> s m-ent)
              (recur s r (inc index) code)
              (recur m-ent r (inc index) guess)))
          guess)))))

(defrecord random-guesser []
  Guesser
  (init-memory [_])
  (make-guess [this] (random-guess))
  (add-hint [_ _ _]))

(defrecord r-guesser [mem]
  Guesser
  (init-memory [this] (reset! mem {}))
  (make-guess [this] (remembering-guess @mem))
  (add-hint [this guess hint] (swap! mem assoc guess hint)))

(defrecord max-ent-guesser [probability-distribution]
  Guesser
  (init-memory [this] (reset! probability-distribution initial-distribution))
  (make-guess [this] (max-entropy-guess @probability-distribution))
  (add-hint [this guess hint] (try+
                                (swap! probability-distribution bayesian-update guess hint)
                                (catch [:type ::inconsistent-hint] _
                                  (println "Inconsistent hint caught")))))


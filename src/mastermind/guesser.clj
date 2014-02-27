(ns mastermind.guesser
  (:require [clojure.set :refer [intersection]]
            [mastermind.game :refer [Guesser]]
            [slingshot.slingshot :refer [throw+ try+]]))

(defn random-guess []
  (let [random-char (partial rand-nth [\A \B \C \D \E \F])]
    (apply str (repeatedly 4 random-char))))

(defn- update-first [coll pred f]
  (let [[first-part [target & r]] (split-with (complement pred) coll)]
    (concat first-part [(f target)] r)))

(defn- count-position-matches [guess secret]
  (count (filter (partial apply =) (map vector guess secret))))

(defn count-value-matches [guess secret]
  (letfn [(count-occ [x coll]
            (count (filter (partial = x) coll)))]
    (reduce + (map (fn [x]
                     (min (count-occ x guess)
                          (count-occ x secret))) (set guess)))))

(defn- ^:testable score [guess secret]
  (let [correct (count-position-matches guess secret)
        matches (count-value-matches guess secret)]
    [correct (- matches correct)]))

(defn- ^:testable compatible? [new-guess [guess hint]]
  (= (score guess new-guess) hint))

(defn- consistent-with-history? [new-guess state]
  (every? (partial compatible? new-guess) state))

(def all-combinations
  (let [letters [\A \B \C \D \E \F]]
    (for [n1 letters n2 letters n3 letters n4 letters]
      (str n1 n2 n3 n4))))

(defn remembering-guesser [history]
  (let [find-comb (fn [guess] (when (consistent-with-history? guess history)
                                guess))
        letters [\A \B \C \D \E \F]
        combination (some find-comb all-combinations)]
    combination))

(def p-dist (zipmap all-combinations (repeat 1296 1/1296)))

(defn- normalize [dist]
  (try+
    (let [total (reduce + (vals dist))]
      (persistent! (reduce (fn [m [k v]] (assoc! m k (/ v total))) (transient {}) dist)))
    (catch ArithmeticException e
      (throw+ {:type ::inconsistent-hint}))))

(defn- ^:testable bayesian-update [dist guess hint]
  (letfn [(likelihood [code guess hint] (if (= (score guess code) hint) 1 0))
          (posterior [m [code _]] (assoc! m code (if (zero? (dist code)) 0 (likelihood code guess hint))))]
    (normalize (persistent! (reduce posterior (transient {}) dist)))))

(defn- ^:testable entropy [dist]
  (- (reduce + (map (fn [[k v]] (if (zero? v) 0 (* v (Math/log v)))) dist))))

(defn- ^:testable score-dist [dist guess]
  (reduce (fn [m [code prob]] (merge-with + m {(score code guess) prob})) {} dist))

(defn max-entropy-guess [dist]
  (let [sd (partial score-dist dist)
        ent (fn [code] (if (= (dist code) 1) 10 (entropy (sd code))))]
    (apply max-key ent all-combinations)))

(defrecord random-guesser []
  Guesser
  (init-memory [_])
  (make-guess [this] (random-guess))
  (add-hint [_ _ _]))

(defrecord r-guesser [mem]
  Guesser
  (init-memory [this] (reset! mem {}))
  (make-guess [this] (remembering-guesser @mem))
  (add-hint [this guess hint] (swap! mem assoc guess hint)))

(defrecord max-ent-guesser [probability-distribution]
  Guesser
  (init-memory [this] (reset! probability-distribution p-dist))
  (make-guess [this] (max-entropy-guess @probability-distribution))
  (add-hint [this guess hint] (try+
                                (swap! probability-distribution bayesian-update guess hint)
                                (catch [:type ::inconsistent-hint] _
                                  (println "Inconsistent hint caught")))))


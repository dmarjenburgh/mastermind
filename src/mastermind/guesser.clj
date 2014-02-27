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
    (vec
      (for [n1 letters n2 letters n3 letters n4 letters]
        (str n1 n2 n3 n4)))))

(def all-scores
  (vec
    (for [black (range 5) white (range (- 5 black))]
      [black white])))

(defn remembering-guess [history]
  (let [find-comb (fn [guess] (when (consistent-with-history? guess history)
                                guess))
        letters [\A \B \C \D \E \F]
        combination (some find-comb all-combinations)]
    combination))

(def p-dist (vec (repeat 1296 (float 1/1296))))

(defn code->index [code]
  (reduce #(+ (* 6 %) (- (long %2) 65)) 0 code))

(defn index->code [index]
  (nth all-combinations index))

(defn score->index [[black white]]
  (+ white (/ (* (- 11 black) black) 2)))

(defn index->score [index]
  (nth all-scores index))

(defn normalise [^doubles dist]
  (let [sum (reduce + dist)]
    (mapv (fn [^double x] (/ x sum)) dist)))

(defn- ^:testable bayesian-update [dist guess hint]
  (letfn [(likelihood [code] (if (= (score guess code) hint) 1 0))
          (posterior [v index prob] (assoc! v index (if (zero? prob) 0 (likelihood (index->code index)))))]
    (normalise (persistent! (reduce-kv posterior (transient []) dist)))))

(defn- ^:testable entropy [dist]
  (- (reduce + (mapv (fn [p] (if (zero? p) 0 (* p (Math/log p)))) dist))))

(defn- ^:testable score-dist [dist guess]
  (reduce-kv (fn [v index prob] (update-in v [(score->index (score (index->code index) guess))] + prob))
             (vec (repeat 15 0)) dist))

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
      (loop [m-ent 0 [code & r] all-combinations index 0 guess nil]
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
  (init-memory [this] (reset! probability-distribution p-dist))
  (make-guess [this] (max-entropy-guess @probability-distribution))
  (add-hint [this guess hint] (try+
                                (swap! probability-distribution bayesian-update guess hint)
                                (catch [:type ::inconsistent-hint] _
                                  (println "Inconsistent hint caught")))))


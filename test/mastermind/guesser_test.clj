(ns mastermind.guesser-test
  (:require [midje.sweet :refer :all]
            [midje.util :refer [expose-testables]]
            [mastermind.guesser :refer :all]
            [mastermind.strategy :refer [initial-distribution code->index score->index]]))

(expose-testables mastermind.guesser)

(def dist-1 (bayesian-update initial-distribution "ABCD" [0 0]))

(facts "about `bayesian-update`"
       (let [posterior (bayesian-update initial-distribution "AAAA" [1 0])
             post-prob (fn [code] (nth posterior (code->index code)))]
         (post-prob "ABCD") => (roughly 1/500)
         (post-prob "BCDF") => (roughly 0))
       (let [posterior (bayesian-update initial-distribution "ABCD" [0 0])
             post-prob (fn [code] (nth posterior (code->index code)))]
         (post-prob "EEEE") => (roughly 1/16)
         (post-prob "EBAC") => (roughly 0))
       (fact "order of information gain is irrelevant"
             (let [posterior (-> (bayesian-update initial-distribution "ABCD" [0 0])
                                 (bayesian-update "EEEE" [1 0]))
                   post-prob (fn [code] (nth posterior (code->index code)))]
               (post-prob "EFFF") => (roughly 1/4)))
       (fact "near solution"
             (let [posterior (bayesian-update dist-1 "AEEF" [0 2])
                   post-prob (fn [code] (nth posterior (code->index code)))]
               (post-prob "FFFE") => (roughly 1)
               (post-prob "EFFF") => (roughly 0))))

#_(facts "about `max-entropy-guess`"
       (let [posterior (bayesian-update initial-distribution "ABCD" [0 0])]
         (max-entropy-guess posterior) => "FFED"
         ))

(facts "about `score-dist`"
       (let [sd (score-dist initial-distribution "AAAA")]
         (tabular
           (facts (sd (score->index ?a)) => (roughly ?b))
           ?a    ?b
           [0 0] 625/1296
           [1 0] 500/1296
           [2 0] 150/1296
           [3 0] 20/1296
           [4 0] 1/1296))
       (let [sd (score-dist initial-distribution "ABCD")]
         (tabular
           (facts (sd (score->index ?a)) => (roughly ?b))
           ?a    ?b
           [0 0] 16/1296
           [0 1] 152/1296
           [0 2] 312/1296
           [0 3] 136/1296
           [0 4] 9/1296
           [1 0] 108/1296
           [2 0] 96/1296
           [3 0] 20/1296
           [4 0] 1/1296))
       (let [sd (score-dist dist-1 "EEEE")]
         (tabular
           (facts (sd (score->index ?a)) => (roughly ?b))
           ?a    ?b
           [0 0] 1/16
           [1 0] 4/16
           [2 0] 6/16
           [3 0] 4/16
           [4 0] 1/16)))

(comment

  (entropy (score-dist dist-1 "AEEF"))
  (entropy (score-dist dist-1 "EBEF"))
  (entropy (score-dist dist-1 "FFED"))

  )

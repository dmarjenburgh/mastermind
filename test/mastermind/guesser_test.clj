(ns mastermind.guesser-test
  (:require [midje.sweet :refer :all]
            [midje.util :refer [expose-testables]]
            [mastermind.guesser :refer :all]))

(expose-testables mastermind.guesser)

(def dist-1 (bayesian-update p-dist "ABCD" [0 0]))

(facts "about `score`"
       (score "AAAA" "AAAA") => [4 0]
       (score "AAAB" "AAAA") => [3 0]
       (score "ABCD" "XBCX") => [2 0]
       (score "XXXX" "ABCD") => [0 0]
       (score "ABCD" "BCDA") => [0 4]
       (score "ABDC" "ABCD") => [2 2]
       (score "AABB" "ABBA") => [2 2])

(facts "about `compatible?`"
       (fact "Only the same guess is compatible with a [4 0] hint"
             (compatible? "ABCD" ["ABCD" [4 0]]) => true
             (compatible? "ABCE" ["ABCD" [4 0]]) => false)
       (fact "An empty hint means all characters must be different"
             (compatible? "DEFF" ["ABCC" [0 0]]) => true
             (compatible? "ACBD" ["BFFE" [0 0]]) => false)
       (fact "n plusses means exactly n matching positions"
             (compatible? "ABBC" ["ADFC" [2 0]]) => true)
       (fact "all - means a permutation without fixed points"
             (compatible? "BCDA" ["ABCD" [0 4]]) => true
             (compatible? "ADBC" ["ABCD" [0 4]]) => false
             (compatible? "BCDE" ["ABCD" [0 4]]) => false))

(facts "normalise"
       (normalise [1 2 3 4]) => [0.1 0.2 0.3 0.4])

(facts "code->index and index->code"
       (code->index "AAAA") => 0
       (code->index "AAAB") => 1
       (code->index "ABCD") => 51
       (code->index "FFFF") => 1295
       (let [c->i->c (comp index->code code->index)
             i->c->i (comp code->index index->code)]
         (c->i->c "CBDA") => "CBDA"
         (i->c->i 846) => 846))

(facts "score->index and index->score"
       (score->index [0 0]) => 0
       (score->index [0 1]) => 1
       (score->index [2 2]) => 11
       (score->index [4 0]) => 14
       (let [s->i->s (comp index->score score->index)
             i->s->i (comp score->index index->score)]
         (s->i->s [1 3]) => [1 3]
         (i->s->i 7) => 7))

(facts "about `bayesian-update`"
       (let [posterior (bayesian-update p-dist "AAAA" [1 0])
             post-prob (fn [code] (nth posterior (code->index code)))]
         (post-prob "ABCD") => (roughly 1/500)
         (post-prob "BCDF") => (roughly 0))
       (let [posterior (bayesian-update p-dist "ABCD" [0 0])
             post-prob (fn [code] (nth posterior (code->index code)))]
         (post-prob "EEEE") => (roughly 1/16)
         (post-prob "EBAC") => (roughly 0))
       (fact "order of information gain is irrelevant"
             (let [posterior (-> (bayesian-update p-dist "ABCD" [0 0])
                                 (bayesian-update "EEEE" [1 0]))
                   post-prob (fn [code] (nth posterior (code->index code)))]
               (post-prob "EFFF") => (roughly 1/4)))
       (fact "near solution"
             (let [posterior (bayesian-update dist-1 "AEEF" [0 2])
                   post-prob (fn [code] (nth posterior (code->index code)))]
               (post-prob "FFFE") => (roughly 1)
               (post-prob "EFFF") => (roughly 0))))

#_(facts "about `max-entropy-guess`"
       (let [posterior (bayesian-update p-dist "ABCD" [0 0])]
         (max-entropy-guess posterior) => "FFED"
         ))

(facts "about `score-dist`"
       (let [sd (score-dist p-dist "AAAA")]
         (tabular
           (facts (sd (score->index ?a)) => (roughly ?b))
           ?a    ?b
           [0 0] 625/1296
           [1 0] 500/1296
           [2 0] 150/1296
           [3 0] 20/1296
           [4 0] 1/1296))
       (let [sd (score-dist p-dist "ABCD")]
         (tabular
           (facts (sd (score->index ?a)) => (roughly ?b))
           ?a   ?b
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
           ?a   ?b
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

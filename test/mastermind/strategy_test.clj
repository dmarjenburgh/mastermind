(ns mastermind.strategy-test
  (:require [midje.sweet :refer :all]
            [midje.util :refer [expose-testables]]
            [mastermind.strategy :refer :all]))

(expose-testables mastermind.strategy)

(facts "normalise"
       (normalise [1.0 2.0 3.0 4.0]) => [0.1 0.2 0.3 0.4])

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


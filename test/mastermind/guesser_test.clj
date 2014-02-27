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

(facts "about `bayesian-update`"
       (let [posterior (bayesian-update p-dist "AAAA" [1 0])]
         (posterior "ABCD") => 1/500
         (posterior "BCDF") => 0)
       (let [posterior (bayesian-update p-dist "ABCD" [0 0])]
         (posterior "EEEE") => 1/16
         (posterior "EBAC") => 0)
       (fact "order of information gain is irrelevant"
             (let [posterior (-> (bayesian-update p-dist "ABCD" [0 0])
                                 (bayesian-update "EEEE" [1 0]))]
               (posterior "EFFF") => 1/4))
       (fact "near solution"
             (let [posterior (bayesian-update dist-1 "AEEF" [0 2])]
               (posterior "FFFE") => 1
               (posterior "EFFF") => 0)))

#_(facts "about `max-entropy-guess`"
       (let [posterior (bayesian-update p-dist "ABCD" [0 0])]
         ;(max-entropy-guess posterior) => "FFED"
         ))

(facts "about `score-dist`"
       (let [sd (score-dist p-dist "AAAA")]
         (sd [0 0]) => 625/1296
         (sd [1 0]) => 500/1296
         (sd [2 0]) => 150/1296
         (sd [3 0]) => 20/1296
         (sd [4 0]) => 1/1296)
       (let [sd (score-dist p-dist "ABCD")]
         (sd [0 0]) => 16/1296
         (sd [0 1]) => 152/1296
         (sd [0 2]) => 312/1296
         (sd [0 3]) => 136/1296
         (sd [0 4]) => 9/1296
         (sd [1 0]) => 108/1296
         (sd [2 0]) => 96/1296
         (sd [3 0]) => 20/1296
         (sd [4 0]) => 1/1296)
       (let [sd (score-dist dist-1 "EEEE")]
         (sd [0 0]) => 1/16
         (sd [1 0]) => 4/16
         (sd [2 0]) => 6/16
         (sd [3 0]) => 4/16
         (sd [4 0]) => 1/16))

(comment

  (entropy (score-dist dist-1 "AEEF"))
  (entropy (score-dist dist-1 "EBEF"))
  (entropy (score-dist dist-1 "FFED"))

  )

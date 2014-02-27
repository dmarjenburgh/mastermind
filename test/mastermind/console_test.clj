(ns mastermind.console-test
  (:require [midje.sweet :refer :all]
            [mastermind.game :refer :all]
            [mastermind.console :refer :all]))

(facts "about console"
       (let [my-console (->console)]
         (fact
           (with-out-str
             (announce-winning-code my-console "ABCD")) => "The code was: ABCD\n"
           (with-out-str
             (announce-tries my-console 8)) => "It took 8 tries.\n")))


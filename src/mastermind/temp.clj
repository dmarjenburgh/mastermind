(ns mastermind.temp
  (:require [clojure.walk :refer [macroexpand-all]]))

(defn- send! [addr data callback]
  (println "Sending to" addr)
  (callback {:data data :address addr}))

(defn- reply! [addr data]
  (println "`replying to" addr)
  {:data data :address addr})

(fn [message]
  (send! "1" message (fn [res1]
                       (send! "2" res1 (fn [res2]
                                         (send! "3" (fn [res3]
                                                      (reply! message res3))))))))



(comment

  (~> message (f1 "1") (f2 "2") (f3 "3") (f4 message))

  (send! "1" (f1 message)
         (fn [res]
           (send! "2" (f2 res)
                  (fn [res]
                    (send! "3" (f3 res)
                           (fn [res]
                             (reply message (f4 res))))))))

  (async-comp message) => message
  (async-comp message (f1 "1")) => (reply! "1" (f1 message))
  (async-comp message (f1 "1") (f2 "2")) => (send! "1" (f1 message)
                                                   (fn [res]
                                                     (reply! "2" (f2 res))))

  )

(defmacro async-comp
  ([msg] msg)
  ([msg [f addr]]
   `(reply! ~addr (~f ~msg)))
  ([msg [f addr] & more]
   `(send! ~addr (~f ~msg) (fn [result#] (async-comp result# ~@more)))))

(defmacro async-comp2
  ([msg] msg)
  ([msg f]
   `(reply! ~msg (~f ~msg)))
  ([msg f & more]
   `(send! ~msg (~f ~msg) (fn [res#] (async-comp res# ~@more)))))

(macroexpand '(async-comp message))
(macroexpand '(async-comp message (f1 "1")))
(macroexpand '(async-comp message (f1 "1") (f2 "2")))
(macroexpand '(async-comp message (f1 "1") (f2 "2") (f3 "3")))
(macroexpand '(async-comp message (f1 "1") (f2 "2") (f3 "3") (f4 message)))
(macroexpand-all '(async-comp message (f1 "1") (f2 "2") (f3 "3") (f4 message)))

#_(macroexpand-all '(async-comp2 message f1))

(defn- authorize-request [req]
  (println "Authorizing request" (:data req))
  :granted)

(defn- retrieve-db-data [{access-rights :data}]
  (case access-rights
    :granted {:height 190 :weight 80}
    :denied nil))

(defn- do-stuff [data]
  (println "Height is" 190 "cm")
  {:success true :status 200 :data data})

(macroexpand-all '(async-comp {:data "Some request" :addr 1} (authorize-request "1") (retrieve-db-data "2") (do-stuff "some-request")))


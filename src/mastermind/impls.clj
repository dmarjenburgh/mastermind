(ns mastermind.impls
  (:require [mastermind.protocols :refer [MyProto]]))

(defrecord ProtoImpl []
  MyProto
  (proto [this n] (println "Called with n")))



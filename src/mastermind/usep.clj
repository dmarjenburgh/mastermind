(ns mastermind.usep
  (:require [mastermind.protocols :refer [proto]]
            [mastermind.impls :refer [->ProtoImpl]])
  (:import [mastermind.impls ProtoImpl]))

(proto (->ProtoImpl) 3)

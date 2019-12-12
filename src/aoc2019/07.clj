(ns aoc2019.07
  (:require [clojure.string :as str]
            [aoc2019.utils :as U]
            [clojure.set :as set]
            [aoc2019.intcode2 :as ic]
            [clojure.math.combinatorics :as combo]))

;(println (vec (map (comp str U/to-ints) (combo/permutations "123"))))
(defn get-perms [s]
  (let [perms (combo/permutations s)]
    (for [c perms]
      (U/to-ints (map str c)))
    )
  )
()


(defn aoc7-1 [instructions ^String inputs]
  (let [perms (combo/permutations inputs)]


    (ic/process instructions inputs)
    )
  )
(defn process-perm [instructions perm]
  ()
  )
(println (get-perms "123"))
(def data (ic/parse-data "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"))
(println @ic/output-reg)
;(println (ic/process (ic/parse-data "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0") [0, 1, 2, 3, 4]))
;(println ())
;(println (apply (map str) combos))
;(println (ic/parse-data "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))


(ns aoc2019.02
  (:require [aoc2019.intcode :as ic]))

(defn aoc2-b [opcodes]
  (first
    (for [x (range 100) y (range 100)
          :let [res (+ (* 100 x) y)]
          :when (= 19690720 (-> (assoc opcodes 1 x 2 y) ic/process first))]
      res)
    )
  )

;(println (ic/parse-file "02"))
(println (-> (assoc (ic/parse-file "02") 1 12 2 2) ic/process first))
(println (aoc2-b (ic/parse-file "02")))

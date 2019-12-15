(ns aoc2019.09
  (:require [aoc2019.intcode2 :as ic]))


(reset! ic/relative-base 0)
;(println (ic/parse-data "104,1125899906842624,99"))
(println (ic/process (ic/parse-data "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")))

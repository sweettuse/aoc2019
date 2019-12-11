(ns aoc2019.05
  (:require [aoc2019.intcode2 :as ic]))

(ic/process (ic/parse-file "05") [1])
(ic/process (ic/parse-file "05") [5])

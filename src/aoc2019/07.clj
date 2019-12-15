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


(defn process-1 [instructions cur-perm]
  (loop [perm cur-perm
         prev-output 0]
    (let [[p & r-perm] perm]
      (if (empty? perm)
        ;(println @ic/output-reg)
        (do (println)
            @ic/output-reg)
        (do (ic/process instructions [p prev-output])
            (recur r-perm @ic/output-reg)
            )
        )
      )
    )
  )
(defn aoc7 [instructions ^String inputs process-func]
  (reset! ic/output-reg 0)
  (apply max
         (for [cur-perm (get-perms inputs)]
           (process-func instructions cur-perm)))
  )


(defn get-insts [instructions inst-map a inputs]
  (let [[cur-in & inputs :as full-input] inputs]
    (if-let [settings (a inst-map)]
      [inst-map settings [[] full-input]]
      (let [settings [0 instructions]]
        [(assoc inst-map a settings) settings [[cur-in] inputs]])
      )
    )
  )

(defn endless-amps [amp-str]
  (flatten
    (for [vals (repeat (flatten (map (comp keyword str) amp-str)))]
      (for [v vals] v))
    )
  )

(defn process-2
  "feedback loop of amps"
  [instructions inputs]
  (reset! ic/output-reg 0)
  (loop [amps (endless-amps "ABCDE")
         inst-map {}
         inputs inputs]
    (let [[a & amps] amps
          [inst-map [pc instructions] [cur-in inputs]] (get-insts instructions inst-map a inputs)
          cur-inputs (conj cur-in @ic/output-reg)
          [pc instructions reason] (ic/process instructions cur-inputs pc true)]
      (if (and (= reason :halt) (= a :E))
        @ic/output-reg
        (recur amps (assoc inst-map a [pc instructions]) inputs)
        )
      )
    )
  )

;(println (get-perms "123"))
;(def data (ic/parse-data "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"))
;(def data (ic/parse-data "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
;(println (keyword "jeb"))
;(def data (ic/parse-data "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))
(def data (ic/parse-data "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"))
;(let [[d & other] (endless-amps "abcde")]
;  (println d (second other))
;  )
;(println (first ))
(reset! ic/output-reg 0)
(println "jeb" (process-2 data [9, 7, 8, 5, 6]))
;(println @ic/output-reg)
;(println (ic/process (ic/parse-data "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0") [0, 1, 2, 3, 4]))
;(println ())
;(println (apply (map str) combos))
;(println (ic/parse-data "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))

(println (aoc7 (ic/parse-file "07") "43210" process-1))
(println (aoc7 (ic/parse-file "07") "56789" process-2))
;(println (aoc7 data "56789" process-2))

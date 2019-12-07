(ns aoc2019.02
  (:require [aoc2019.utils :as U]
            [clojure.string :as str]))

; intcode computer
(def funcs {1 + 2 *})

; vector time
(defn parse-file
  ([] (parse-file (first (U/read-file "02"))))
  ([data]
   (vec (U/to-ints (str/split data #","))))
  )


(defn calc [opcodes idx]
  (let [val-at #(opcodes (opcodes %))
        func (funcs (opcodes idx))
        out-idx (opcodes (+ idx 3))]
    (assoc opcodes out-idx (func (val-at (+ idx 1)) (val-at (+ idx 2))))
    )
  )

(defn aoc2-a [opcodes]
  (loop [opcodes opcodes
         idx 0]
    (if (or (>= idx (count opcodes)) (= (opcodes idx) 99))
      (first opcodes)
      (recur (calc opcodes idx) (+ idx 4))
      )
    )
  )

(println (aoc2-a (assoc (parse-file) 1 12 2 2)))

(defn aoc2-b [opcodes]
  (for [x (range 100) y (range 100)
    :let [res (+ (* 100 x) y)]
    :when (= 19690720 (aoc2-a (assoc opcodes 1 x 2 y)))]
    res
    )
  )
(println (aoc2-b (parse-file)))

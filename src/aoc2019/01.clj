(ns aoc2019.01
  (:require [aoc2019.utils :as U]))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn calc-fuel
  "how much fuel is needed for a certain `mass`"
  [mass]
  (- (quot mass 3) 2)
  )


(defn aoc1-a []
  "problem 1"
  (U/sum
    (map calc-fuel
         (U/to-ints (U/read-file "01-a"))
         )
    )
  )

(defn aoc1-a-2 []
  "problem 1"
  (reduce +
          (map (comp calc-fuel #(Integer/parseInt %))
               (U/read-file "01-a")))
  )

(println (= (aoc1-a) (aoc1-a-2)))

(defn calc-fuel2 [mass]
  "calculate fuel required for both masses AND additional mass of fuel"
  (loop [mass mass
         acc 0]
    (let [mass (calc-fuel mass)]
      (if (<= mass 0)
        acc
        (recur mass (+ acc mass)))
      )
    )
  )

(defn aoc1-b []
  (U/sum (map calc-fuel2 (U/to-ints (U/read-file "01-a"))))
  )

(println (calc-fuel2 100756))
(println (aoc1-b))

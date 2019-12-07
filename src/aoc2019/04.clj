(ns aoc2019.04
  (:require [aoc2019.utils :as U]
            [clojure.string :as str]
            [clojure.set :as set]))

(def r (range 402328 864248))
;(def r (range 402328 444456))

(defn has-double [p]
  (= (p 0) (p 1))
  )
(defn has-double2
  ([s] has-double2 s false)
  ([s exact]
   (let [compar (if exact = >=)]
     (some true?
           (for [c (set s)]
             (compar (count (filter (partial = c) s)) 2)
             )
           )
     )
   )
  )


(defn is-monotonic [p]
  (>= (p 1) (p 0))
  )

(defn valid
  ([n] (valid n false))
  ([n exact]
   (let [s (map int (str n))
         pairs (map vector s (rest s))]
     (and
       (has-double2 s exact)
       (every? true? (map is-monotonic pairs))
       )
     )
   )
  )

(defn aoc4-a
  ([] (aoc4-a false))
  ([exact]
   (count
     (filter true?
             (map #(valid % exact) r)))
   )
  )

;(println (has-double [1 2]))
;(println (map (juxt has-double is-monotonic) [[1 2] [3 3]]))
;(println (map int "123456"))

;(println (any? [nil nil nil]))
;(println (valid 444555 true))
(println (aoc4-a true))

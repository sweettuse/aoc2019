(ns aoc2019.03
  (:require [aoc2019.utils :as U]
            [clojure.string :as str]
            [clojure.set :as set]))

; sample: R8,U5,L5,D3
(def dir-map
  {"R" {:func + :axis :x}
   "U" {:func + :axis :y}
   "L" {:func - :axis :x}
   "D" {:func - :axis :y}}
  )

(defn parse-file
  ([] (parse-file (U/read-file "03")))
  ([data]
   (map #(str/split % #",") data))
  )

(defn parse-inst [inst point]
  (let [dir (dir-map (str (first inst)))
        magnitude (Integer/parseInt (apply str (rest inst)))]
    (update-in point [(:axis dir)] (:func dir) magnitude)
    )
  )

(defn Point [x y] {:x x :y y})
(defn to-points [insts]
  (loop [insts insts
         res [(Point 0 0)]]
    (if (empty? insts)
      res
      (recur (rest insts) (conj res (parse-inst (first insts) (last res))))
      )
    )
  )


(def subset
  (memoize
    (fn [p1 p2 kw]
      (let [[inc-dec step] (if (< (kw p1) (kw p2)) [inc 1] [dec -1])]
        (range (kw p1) (inc-dec (kw p2)) step)
        )
      )
    )
  )


(defn point-vals [p1 p2]
  "return set of x and y vals"
  (for [x (subset p1 p2 :x)
        y (subset p1 p2 :y)
        :let [res (Point x y)]]
    res)
  )


(defn intersect [p1 p2 p3 p4]
  (clojure.set/intersection (set (point-vals p1 p2)) (set (point-vals p3 p4)))
  )


(defn get-intersection-points [data]
  (let [l1 (to-points (first data))
        l2 (to-points (last data))
        l1-points (map vector l1 (take 1000 (rest l1)))
        l2-points (map vector l2 (take 1000 (rest l2)))]
    [(disj
       (reduce set/union
               (for [[p1 p2] l1-points
                     [p3 p4] l2-points]
                 (intersect p1 p2 p3 p4))
               )
       {:x 0 :y 0}
       ),
     l1,
     l2]
    )
  )


(defn aoc3-a [data]
  (let [[points & _] (get-intersection-points data)]
    (apply min
           (map #(+ (Math/abs (:x %)) (Math/abs (:y %))) points))
    )
  )


(def test-data ["R75,D30,R83,U83,L12,D49,R71,U7,L72",
                "U62,R66,U55,R34,D71,R55,D58,R83"])

(println (aoc3-a (parse-file test-data)))

(def expand-wire
  (memoize
    (fn [l]
      (for [[p1 p2] (map vector l (rest l))
            :let [res (point-vals p1 p2)]]
        (flatten res)
        )
      )
    )
  )

(defn calc-distance [point l]
  "calc number of steps to reach intersection point"
  (count (take-while (partial not= point) l))
  )


(defn aoc3-b [data]
  (let [[points l1 l2] (get-intersection-points data)
        l1-points (flatten (map butlast (expand-wire l1)))
        l2-points (flatten (map butlast (expand-wire l2)))
        ]
    ;(println points)
    ;(println l1)
    ;(println l2)
    (apply min
           (map #(+ (calc-distance % l1-points) (calc-distance % l2-points)) points)
           )
    )
  )

(def test-data2 ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])
(println (aoc3-b (parse-file)))


























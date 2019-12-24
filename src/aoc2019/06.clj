(ns aoc2019.06
  (:require [clojure.string :as str]
            [aoc2019.utils :as U]
            [clojure.set :as set]))


(defn- parse-file [filename]
  "return a map of {body->set(orbiters)}"
  (let [data (U/read-file filename)]
    (reduce (fn [acc s]
              (let [[center target] (map keyword (str/split s #"\)"))]
                (merge-with set/union acc {center #{target}}))
              )
            {}
            data
            )
    )
  )

(defn- get-counts-recursive
  ([planet-map] (get-counts-recursive planet-map 0 :COM))
  ([planet-map num-parents current]
   (let [f (partial get-counts-recursive planet-map (inc num-parents))]
     (if-let [current (current planet-map)]
       (apply + num-parents (map f current))
       num-parents
       )
     )
   )
  )


(defn- to-count-args [n-parents current]
  {:n-parents n-parents :current current}
  )

(defn- from-count-args [ca]
  [(:n-parents ca) (:current ca)])

(defn- get-counts-args [planet-map args-vec]
  "write own stack to handle stack overflow errors"
  (loop [acc 0
         args-vec args-vec]

    (if (empty? args-vec)
      acc
      (let [[n-parents current] (from-count-args (first args-vec))
            acc (+ acc n-parents)
            new-args (for [c (current planet-map [])
                           :let [res (to-count-args (inc n-parents) c)]]
                       res)
            args-vec (concat (rest args-vec) new-args)]
        (recur acc args-vec)))))

(defn aoc6-a [filename]
  (get-counts-args (parse-file filename) [(to-count-args 0 :COM)])
  )


; ======================================================================================================================
(defn create-node [parent current]
  {:parent parent :name current}
  )


(defn- parse-file2 [filename]
  "return a map of {body->set(orbiters)}"
  (let [data (U/read-file filename)
        child-map (parse-file filename)
        parent-map
        (reduce (fn [acc s]
                  (let [[parent orbiter] (map keyword (str/split s #"\)"))]
                    (merge-with #(merge-with set/union %&) acc {orbiter parent}))
                  )
                {}
                data
                )
        ]
    [child-map, parent-map]

    )
  )

(defn get-parents
  "get all of body's parents from closest to furthest"
  [parent-map name]
  (loop [name name
         acc []]
    (if-let [parent (name parent-map)]
      (recur parent (conj acc parent))
      acc
      )
    )
  )


(defn- shared-ancestors [you santa]
  (set/intersection (set you) (set santa))
  )

(defn- to-value-map [parents]
  (apply hash-map (interleave parents (U/py-count)))
  )

(defn- aoc6-b [filename]
  (let [[_ parent-map] (parse-file2 filename)
        your-parents (get-parents parent-map :YOU)
        santa-parents (get-parents parent-map :SAN)
        shared-parents (shared-ancestors your-parents santa-parents)
        your-p-vals (to-value-map your-parents)
        santa-p-vals (to-value-map santa-parents)
        [closest-p your-dist] (apply min-key second (select-keys your-p-vals shared-parents))
        ]
    (+ your-dist (closest-p santa-p-vals))
    )
  )


(println (aoc6-a "06"))
(println (aoc6-b "06"))


















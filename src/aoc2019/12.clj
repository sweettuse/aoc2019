(ns aoc2019.12
  (:require [aoc2019.utils :as U]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn parse-row [row]
  (let [stripped (str/replace row #"[<>,=]" "")]
    (into {}
          (for [s (str/split stripped #" ")
                :let [name (keyword (str (first s)))
                      val (Integer/parseInt (apply str (rest s)))]]
            [name val]))))


(defn parse-data [data]
  (into {}
        (for [[name row] (map vector "ABCDE" data)
              :let [pos (parse-row row)
                    vel (apply hash-map (interleave (keys pos) (repeat 0)))]]
          {(keyword (str name)) {:pos pos :vel vel}})))


(defn parse-file
  ([] (parse-file "12"))
  ([fn] (parse-data (U/read-file fn))))


(defn- update-velocity [m1 m2]
  "account for gravity and update velocity"
  (println m1 m2)
  (loop [m1 m1
         m2 m2
         ks (keys (:pos m1))]
    (println ks)
    (if (empty? ks)
      m1
      (let [[k & ks] ks
            m1p (k (:pos m1))
            m2p (k (:pos m2))
            update-f (if (< m1p m2p) inc dec)]
        (recur (update-in m1 [:vel k] update-f) m2 ks)))))


(defn update-system-position [m]
  "take mass with :pos and :vel and update position"
  (assoc m :pos (apply merge-with + (vals m))))


(defn update-system-velocity
  ([] (update-system-velocity (parse-file)))
  ([data]
   (loop [[[k1 k2] & ks] (combo/combinations (keys data) 2)
          data data]
     (if (empty? ks)
       ; some sort of reduce here to update the positions appropriately based on velocities
       data
       (recur ks (assoc data k1 (update-velocity (k1 data) (k2 data))))))))


(def data (parse-file))
(println (keys data))
(def m1 (:A data))
(def m2 (:B data))
(println (update-velocity m1 m2))
(println m1)
(def res (update-system-velocity (parse-file "12.test")))
(println (:A res))
(println (apply merge-with + (vals (:A res))))


  ;(println (first (keys (first data))))



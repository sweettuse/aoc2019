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



(defn init-system-vel [data]
  "set :vel to all zeroes"
  (reduce (fn [acc k]
            (let [pos-vel (k data)
                  pos-keys (keys (:pos pos-vel))
                  vel (zipmap pos-keys (repeat 0))]
              (assoc acc k (assoc pos-vel :vel vel))))
          {}
          (keys data)))


(defn parse-data [data]
  (init-system-vel
    (into {}
          (for [[name row] (map vector "ABCDE" data)
                :let [pos (parse-row row)]]
            ;{(keyword (str name)) (init-vel {:pos pos})})))
            {(keyword (str name)) {:pos pos}}))))


(defn parse-file
  ([] (parse-file "12"))
  ([fn] (parse-data (U/read-file fn))))


(defn- update-velocity [m1 m2]
  "account for gravity and update velocity"
  (loop [m1 m1
         m2 m2
         ks (keys (:pos m1))]
    (if (empty? ks)
      m1
      (let [[k & ks] ks
            m1p (k (:pos m1))
            m2p (k (:pos m2))
            update-f (if (< m1p m2p) inc dec)]
        (recur (update-in m1 [:vel k] update-f) m2 ks)))))


(defn update-system-position [data]
  "take mass with :pos and :vel and update position"
  (into {}
        (for [k (keys data)
              :let [cur (k data)]]
          {k (assoc cur :pos (apply merge-with + (vals cur)))})))


(defn update-system
  ([] (update-system (parse-file)))
  ([data]
   (loop [[[k1 k2] & ks] (combo/permutations (keys data) 2)
          data data]
     (if (empty? ks)
       (update-system-position data)
       (recur ks (assoc data k1 (update-velocity (k1 data) (k2 data))))))))


(defn calc-total-energy [data]
  (letfn [(calc-energy [pv]
            ;2
            ;(println pv)
            (U/sum (map #(Math/abs %) (vals pv)))
            )
          (total [moon]
            (println "moon" moon)
            ;(println (U/select-values moon :pos :vel))
            )]
    ;(reduce * (map calc-energy (U/select-values moon :pos :vel))))]
    (map total (vals data))))
;(U/sum (map total data))))

(def data (parse-file "12.test"))
;(def initted (init-system-vel data))
(def updated (update-system data))
(println updated)
(println (calc-total-energy updated))
;(println (keys data))
;(def m1 (:A data))
;(def m2 (:B data))
;(println (update-velocity m1 m2))
;(println m1)
;(def res (update-system (parse-file "12.test")))
;(println res)
;(println "really" (:A res))


;(println (first (keys (first data))))



(ns aoc2019.12-prime
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


;(defn init-system-vel [data]
;  "set :vel to all zeroes"
;  (map #(assoc % :vel (zipmap (keys (:pos %)) (repeat 0))) data))


(defn init-system-vel [data]
  "set :vel to all zeroes"
  (into {}
        (for [pos data
              :let [vel (zipmap (keys pos) (repeat 0))]]
          [pos vel])))


(defn parse-data [data]
  (map parse-row data))


(defn parse-file
  ([] (parse-file "12"))
  ([fn] (parse-data (U/read-file fn))))


(defn- -get-update-funcs [v1 v2]
  (if (= v1 v2)
    [identity identity]
    (if (< v1 v2) [inc dec] [dec inc])))

(defn- -get-update-func [v1 v2]
  (if (= v1 v2)
    identity
    (if (< v1 v2) inc dec)))

(defn- update-velocity [pv1 pv2]
  "account for gravity and update velocity

  m1 and m2 are {pos vel} key-value pairs"
  (loop [pv1 pv1
         pv2 pv2
         ks (keys (first pv1))]
    (if (empty? ks)
      [pv1 pv2]
      (let [[k & ks] ks
            [[p1 v1] [p2 v2]] [pv1 pv2]
            [update-m1 update-m2] (apply -get-update-funcs (map k [p1 p2]))
            new-vel1 (update v1 k update-m1)
            new-vel2 (update v2 k update-m2)
            new-pv1 (assoc pv1 1 new-vel1)
            new-pv2 (assoc pv2 1 new-vel2)]
        (recur new-pv1 new-pv2 ks)))))


(defn- get-velocity-offset [p1 p2]
  "account for gravity and update velocity

  m1 and m2 are [pos vel] key-value vectors"

  (let [coord-keys (keys p1)]
    (reduce
      (fn [acc k]
        (update acc k (-get-update-func (k p1) (k p2))))
      (zipmap coord-keys (repeat 0))
      coord-keys)))

(defn update-system-position [data]
  (into {}
        (for [[k v] data]
          [(merge-with + k v) v])))

(defn update-system
  ([data]
   (loop [combos (combo/combinations (keys data) 2)
          data data]
     ; data at this point looks like {pos1 vel1, pos2 vel2}
     (if (empty? combos)
       (update-system-position data)
       (let [[[p1 p2] & ks] combos
             offset (get-velocity-offset p1 p2)
             v1 (merge-with + (data p1) offset)
             v2 (merge-with - (data p2) offset)]
         (recur ks (assoc data p1 v1 p2 v2)))))))


(defn calc-total-energy [data]
  (println data)
  (letfn [(sum-vals [xyz]
            (reduce + (map #(Math/abs %) (vals xyz))))]
    (apply +
           (for [[p v] (map vector (keys data) (vals data))]
             (* (sum-vals p) (sum-vals v))))))


(defn aoc12-a [n data]
  (loop [n n
         data (init-system-vel data)]
    (if (<= n 0)
      (calc-total-energy data)
      (recur (dec n) (update-system data)))))

(defn aoc12-b [data]
  (loop [n 0
         data (init-system-vel data)
         found #{}]
    (let [h-data (hash data)]
      (when (zero? (mod n 1000000))
        (println n))
      (if (contains? found data)
        n
        (recur (inc n) (update-system data) (conj found data))))))

;(println (aoc12-a 1000 (parse-file "12")))
(println (aoc12-b (parse-file "12.test2")))
;(def data (parse-file "12"))

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
  "add {:x 0 :y 0 :z 0} as initial velocity"
  (into {}
        (for [pos data
              :let [vel (zipmap (keys pos) (repeat 0))]]
          [pos vel])))


(defn parse-data [data]
  (map parse-row data))


(defn parse-file
  ([] (parse-file "12"))
  ([fn] (init-system-vel (parse-data (U/read-file fn)))))


(defn- -get-update-func [v1 v2]
  (if (= v1 v2)
    identity
    (if (< v1 v2) inc dec)))


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
         data data]
    (if (<= n 0)
      (calc-total-energy data)
      (recur (dec n) (update-system data)))))


(defn- by-dimension [data]
  (let [data (flatten (seq data))]
    (into {} (for [dim [:x :y :z]] [dim (map dim data)]))))


(defn- process-dimensions [data seen done]
  (let [data (by-dimension data)]
    (loop [[dim & dims] (keys seen)
           seen seen
           done done]
      (if (not dim)
        [seen done]
        (let [cur-seen (dim seen)
              cur-data (dim data)]
          (if (contains? cur-seen cur-data)
            (recur dims (dissoc seen dim) (assoc done dim cur-seen))
            (recur dims (assoc seen dim (conj cur-seen cur-data)) done)))))))


(defn aoc12-b [data]
  "find period of each axis and then find the lcm of that"
  (loop [data data
         seen (zipmap [:x :y :z] (repeat #{}))
         done {}]
    (let [[seen done] (process-dimensions data seen done)]
      (if (empty? seen)
        (reduce U/lcm (map count (vals done)))
        (recur (update-system data) seen done)))))


(def data (parse-file "12"))
(println (aoc12-a 1000 data))
(println (aoc12-b data))

(ns aoc2019.08
  (:require [clojure.string :as str]
            [aoc2019.utils :as U]
            [clojure.set :as set]))


(def width 25)
(def height 6)
(def per-layer (* width height))


(defn eval-row [data]
  (apply * (vals (select-keys (frequencies data) [1 2])))
  )

;(defn to-layer-freq-info [layers]
;  (for [l layers
;        :let [freq (frequencies l)
;              num-zeroes (freq 0)]]
;    [num-zeroes freq])
;  )
(defn parse-file [filename]
  "return vec of lists of layer data from input"
  (-> filename U/read-file first (#(str/split % #"")) U/to-ints (#(U/chunks % per-layer)))
  )

(defn aoc8-a [filename]
  (let [layers (parse-file filename)
        freqs (map frequencies layers)
        min-layer-freq (first (apply min-key second (for [f freqs] [f (f 0)])))
        ]
    (apply * (vals (select-keys min-layer-freq [1 2])))
    )
  )


(defn aoc8-b [filename]
  (let [layers (parse-file filename)
        pixels (apply map vector layers)]
    (vec
      (U/chunks
        (for [p pixels]
          (first (drop-while (partial = 2) p)))
        25
        )
      )
    )
  )

(println (aoc8-a "08"))
(seq (map println (aoc8-b "08")))


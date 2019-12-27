(ns aoc2019.utils)

(def sum #(reduce + %))
(def input-path "/Users/acushner/software/clj/aoc2019/inputs/")
(defn read-file
  "return list[str] of lines of file"
  ([fn] (read-file fn input-path))
  ([fn path]
   (clojure.string/split-lines (slurp (str path fn)))
   )
  )

(defn to-ints [strs]
  "convert iterable of strings to ints"
  (map #(Integer/parseInt %) strs)
  )

;(defn py-count
;  ([] (py-count 0))
;  ([n] (lazy-seq (cons n (py-count (inc n)))))
;  )

(defn py-count
  ([] (py-count 0))
  ([n] (iterate inc n)))


(defn chunks [data chunk-size]
  (loop [data data
         out []]
    (if (empty? data)
      out
      (recur (drop chunk-size data) (conj out (take chunk-size data)))
      )
    )
  )

(defn select-values [m & keys]
  (vals (select-keys keys m)))

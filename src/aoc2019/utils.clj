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

(defn py-count
  ([] (py-count 0))
  ([n] (lazy-seq (cons n (py-count (inc n)))))
  )

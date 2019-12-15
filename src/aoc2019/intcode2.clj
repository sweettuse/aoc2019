(ns aoc2019.intcode2
  (:require [aoc2019.utils :as U]
            [clojure.string :as str]))

(def output-reg (atom nil))
(def relative-base (atom nil))

(defn- read-val
  ([instructions idx] (read-val instructions idx 0))
  ([instructions idx mode]
   (let [f (case mode
             0 instructions
             1 identity
             2 (comp instructions (partial + @relative-base)))]
     (println "rv" idx mode @relative-base instructions)
     (f (instructions idx)))
   )
  )

(defn write-val [instructions idx value]
  (assoc instructions idx value)
  )

(defn get-args [instructions idxs param-modes]
  (for [[idx pm] (map vector idxs param-modes)
        :let [res (read-val instructions idx pm)]]
    res)
  )


(defn Output
  ([program-counter instructions] (Output program-counter instructions :na))
  ([program-counter instructions reason] [program-counter instructions reason])
  )




; ======================================================================================================================
; OPCODE HANDLERS
; ======================================================================================================================
(defn- make-last-pm-immediate [param-modes]
  "often the last param mode should be immediate. this handles that"
  (assoc param-modes (dec (count param-modes)) 1)
  )

(defn- oc-run-and-write [f instructions idxs param-modes]
  "opcode: take n-args and write out to final idx"
  (let [vals (get-args instructions idxs param-modes)
        [args out-idx] [(butlast vals) (last vals)]]
    (Output
      (inc (last idxs))
      (write-val instructions out-idx (apply f args))
      )
    )
  )

(defn- oc-input [instructions idxs inputs]
  "take user-provided input and write to instructions"
  (let [target-idx (instructions (first idxs))]
    (Output (inc (last idxs)) (write-val instructions target-idx (first inputs)))
    )
  )

(defn- oc-output [instructions idxs param-modes]
  "read and display value from instructions"
  (let [res (read-val instructions (first idxs) (first param-modes))]
    (println "out" res)
    (reset! output-reg res)
    )
  (Output (inc (last idxs)) instructions)
  )

(defn- oc-jump [cmp instructions idxs param-modes]
  "conditional jump"
  (let [[to-compare target-idx] (get-args instructions idxs param-modes)
        pc (if (cmp to-compare) target-idx (inc (last idxs)))]
    (Output pc instructions)
    )
  )

(defn- oc-comp [cmp]
  (fn [& args]
    (if (apply cmp args) 1 0))
  )

(defn- oc-relative-base [instructions idxs param-modes]
  "read and display value from instructions"
  (let [res (read-val instructions (first idxs) (first param-modes))]
    (println "base change" res)
    (swap! relative-base (partial + res))
    )
  (Output (inc (last idxs)) instructions)
  )

(def opcodes
  {1 {:meta "add" :func (partial oc-run-and-write +) :arity 3}
   2 {:meta "mult" :func (partial oc-run-and-write *) :arity 3}
   3 {:meta "write-input" :func oc-input :arity 1}
   4 {:meta "read" :func oc-output :arity 1 :param-mode-override identity}
   5 {:meta "jump-if-nonzero" :func (partial oc-jump #(not (zero? %))) :arity 2 :param-mode-override identity}
   6 {:meta "jump-if-zero" :func (partial oc-jump zero?) :arity 2 :param-mode-override identity}
   7 {:meta "less than" :func (partial oc-run-and-write (oc-comp <)) :arity 3}
   8 {:meta "equal" :func (partial oc-run-and-write (oc-comp =)) :arity 3}
   9 {:meta "relative-base" :func oc-relative-base :arity 1}
   }
  )
; ======================================================================================================================


(defn parse-data [data]
  "transform a string of 'int,int,int,int' into actual ints"
  (vec (map bigint (str/split data #","))))

(defn parse-file [fn]
  "read data from file and parse to ints"
  (parse-data (first (U/read-file fn))))


(defn- parse-opcode [code]
  "of the form [pm2 pm1 pm0 opcode-digit-0 opcode-digit-1]

  return [instruction, (pm0, pm1, pm2)]
  "
  (let [s (format "%05d" (biginteger code))
        opcode (opcodes (mod code 100))]
    [opcode,
     (vec (take (:arity opcode) (reverse (map bigint (map str (take 3 s))))))]
    )
  )

(defn- get-arg-idxs [opcode idx]
  (let [idx (inc idx)
        n-args (:arity opcode)]
    (range idx (+ idx n-args))
    )
  )

(defn process
  ([instructions] (process instructions [] 0 false))
  ([instructions inputs] (process instructions inputs 0 false))
  ([instructions inputs pc break-on-output]
   (loop [pc pc
          instructions instructions
          inputs inputs]
     (if (or (>= pc (count instructions)) (= (instructions pc) 99))
       (Output pc instructions :halt)
       (let [int-code (instructions pc)
             [opcode param-modes] (parse-opcode int-code)
             param-modes ((:param-mode-override opcode make-last-pm-immediate) param-modes)
             idxs (get-arg-idxs opcode pc)]

         (if (= 3 int-code)
           (let [[pc instructions] (oc-input instructions idxs inputs)]
             (recur pc instructions (rest inputs)))
           (let [[pc instructions] ((:func opcode) instructions idxs param-modes)]
             (if (and break-on-output (= 4 int-code))
               (Output pc instructions :break)
               (recur pc instructions inputs)))
           )
         )
       ))))


(defn- test-run []
  ;(let [data (parse-data "3,3,1105,-1,9,1101,0,0,12,4,12,99,1")]
  ;  (process data [0]))
  (let [input "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
        data (parse-data input)]
    (process data [2987]))
  )

;(println (test-run))



(ns aoc2019.intcode
  (:require [aoc2019.utils :as U]
            [clojure.string :as str]))

; ======================================================================================================================
; INTCODE COMPUTER
; ======================================================================================================================

(defn- val-at
  "get value at idx from instructions

  position-mode (parameter-mode 0): treat the value at the idx as another idx, and return that value
  immediate-mode (parameter-mode 1): return the value at that idx"

  ([instructions idx] (val-at instructions idx 0))
  ([instructions idx parameter-mode]
   ((if (zero? parameter-mode) instructions identity) (instructions idx))
   )
  )


(defn- get-args [instructions idx param-modes]
  "read function args from instructio)n set"
  (let [indices (range idx (+ idx (count param-modes)))]
    (for [[idx mode] (map vector indices param-modes)
          :let [res (val-at instructions idx mode)]]
      res
      )
    )
  )

; intcode computer

(defn- write [instructions idx value]
  (assoc instructions (val-at instructions idx 1) value)
  )

(defn Output [pc instructions]
  [pc instructions])

(defn- gen-oc [f instructions idxs pms _]
  "generic opcode that takes n-args, applies them to the function, and writes the result to the
   ihe last arg"
  (let [args (get-args instructions (first idxs) (butlast pms))]
    (Output
      (inc (last idxs))
      (write instructions (last idxs) (apply f args))
      )
    )
  )


(defn- oc3 [& args]
  (apply gen-oc
         (fn [& _] (first (last args)))
         args)
  )



(defn- oc4 [instructions idxs & _]
  (println (val-at instructions (last idxs)))
  (Output
    (inc (last idxs))
    instructions
    )
  )

(defn- oc-comp [f & args]
  (if (apply f args) 1 0)
  )

(defn- oc-jump [cmp instructions idxs pms _]
  (let [args (get-args instructions (first idxs) pms)]
    (Output
      (if (cmp (first args))
        (second args)
        (inc (last idxs)))
      instructions
      )
    )
  )

(def opcodes
  {1 {:func (partial gen-oc +) :arity 3}
   2 {:func (partial gen-oc *) :arity 3}
   3 {:func oc3 :arity 1 :get-input #(rest %)}
   4 {:func oc4 :arity 1}
   5 {:func (partial oc-jump #(not (zero? %))) :arity 2}
   6 {:func (partial oc-jump zero?) :arity 2}
   7 {:func (partial gen-oc (partial oc-comp <)) :arity 3}
   8 {:func (partial gen-oc (partial oc-comp =)) :arity 3}
   }
  )

(defn- parse-opcode [^Integer code]
  "of the form [pm2 pm1 pm0 opcode-digit-0 opcode-digit-1]

  return [instruction, (pm0, pm1, pm2)]
  "
  (let [s (format "%05d" code)
        opcode (opcodes (mod code 100))]
    [opcode,
     (reverse (U/to-ints (map str (take (:arity opcode) s))))]
    )
  )

(defn- process-instruction [instructions idx inputs]
  "get instruction function
  get instruction values
  execute
  return: [new pc, new instructions]
  "
  (let [[opcode param-modes] (parse-opcode (instructions idx))
        indices (range (inc idx) (+ (inc idx) (count param-modes)))]

    ;[(+ idx (inc (:arity opcode))),
    ; ((:func opcode) instructions indices param-modes inputs),
    ; ((:get-input opcode identity) inputs)]
    (conj
      ((:func opcode) instructions indices param-modes inputs)
      ((:get-input opcode identity) inputs))
    )
  )

; vector time
(defn parse-data [^String data]
  "parse data in the form '1,0,0,3,99', e.g."
  (vec (U/to-ints (str/split data #",")))
  )

(defn parse-file [^String fn]
  "read input from file and parse that data"
  (parse-data (first (U/read-file fn)))
  )


(defn process
  ([instructions] (process instructions []))
  ([instructions inputs]
   (loop [[idx instructions inputs] [0 instructions inputs]]
     (if (or (>= idx (count instructions)) (= (instructions idx) 99))
       instructions
       (recur (process-instruction instructions idx inputs))
       )
     )
   )
  )

(defn- test-run []
  ;(process (parse-data "1,9,10,3,2,3,11,0,99,30,40,50"))
  ;(process (parse-data "3,0,99") [1234])
  ;(process (parse-data "3,0,4,0,99") [192934])
  ;(process (parse-data "11108,4,4,0,99"))
  (process (parse-data "3,3,1108,-1,8,3,4,3,99") [8])
  (let [lt "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"]
    (process (parse-data lt) [62])
    )
  )

(test-run)



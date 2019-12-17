(ns z-machine.instruction
  (:require [z-machine.instruction-table :refer :all])
  (:require [z-machine.parser :refer :all]))

(defn decode-operand-types [bytes]
  (defn optype [val]
    (case val
      2r00 :type-large-constant
      2r10 :type-variable
      2r01 :type-small-constant
      2r11 :type-omitted))
  (defn make-bit-pair-sequence [byte]
    [
      (bit-and (bit-shift-right byte 6) 2r11)
      (bit-and (bit-shift-right byte 4) 2r11)
      (bit-and (bit-shift-right byte 2) 2r11)
      (bit-and (bit-shift-right byte 0) 2r11)
    ]
  )
  (defn bit-pair-parser [bit-pair-sequence]
    (let [
      first-pair (first bit-pair-sequence)
      remaining (rest bit-pair-sequence)
      ]
      [[(optype first-pair) remaining]]
      )
  )
  (def parser (sequence-parser (repeat 4 bit-pair-parser)))
  (let [
    [first] bytes
    bit-pairs (make-bit-pair-sequence first)
    ]
    (filter (fn [x] (not= x :type-omitted)) (parse parser bit-pairs))))

(defn extract-operands [operand-types bytes]
    (defn iter [operand-types bytes result]
      (if (empty? operand-types)
        result
        (let [[op & next-ops] operand-types]
          (case op
            :type-large-constant (iter
              next-ops
              (drop 2 bytes)
              (conj result [:type-large-constant (first bytes) (second bytes)]))
            :type-small-constant (iter
              next-ops
              (drop 1 bytes)
              (conj result [:type-small-constant (first bytes)]))
            :type-variable (iter
              next-ops
              (drop 1 bytes)
              (conj result [:type-variable (first bytes)]))))))
    (iter operand-types bytes []))

(defn decode-branch [bytes]
  (let [
    [first second] bytes
    top-bit (bit-and (bit-shift-right first 7) 2r1)
    second-highest-bit (bit-and (bit-shift-right first 6) 2r1)
    branch-on-true (= 2r1 top-bit)
    has-two-bytes (= 2r0 second-highest-bit)
    offset (vec (concat [(bit-and first 2r00111111)] (if has-two-bytes [second] nil)))
  ]
  [branch-on-true offset]))

(defn parse-tail [instruction bytes]
  (def parsers [
    [:store (fn [bs] [(first bs) (rest bs)])]
    [:branch (fn [bs] [(decode-branch bs) nil])]
  ])
  (defn do [[result remainder] [name fn]]
    (if (get instruction name) (fn remainder) [nil remainder]))
  (->>
    (reductions do [[] bytes] parsers)
    (rest)
    (map first)))

(defn count-operand-bytes [operands]
  (->> operands
    (map first)
    (map (fn [type] (if (= type :type-large-constant) 2 1)))
    (reduce +)))

(defn get-tail [bytes-till-operands operands bytes]
  (drop (+ bytes-till-operands (count-operand-bytes operands)) bytes))

(defn count-text2print-bytes [bytes]
  (let [
    [first second & rest] bytes
    top-bit (bit-and (bit-shift-right first 7) 2r1)
    is-last (= top-bit 2r1)]
    (if is-last 2 (+ 2 (count-text2print-bytes rest)))))

(defn text2print-decode [bytes]
  (if (empty? bytes) [] (let [
    word (+ (bit-shift-left (first bytes) 8) (second bytes))
    first-char (bit-shift-right (bit-and word  2r0111110000000000) 10)
    second-char (bit-shift-right (bit-and word 2r0000001111100000) 5)
    third-char (bit-and word  2r0000000000011111)
    rest (drop 2 bytes)
  ] (concat [first-char second-char third-char] (text2print-decode rest)))))

(defn make-long-form [bytes]
  (let [
    [first second third & tail] bytes
    instruction (instruction-table first)
    operands (cond
      (< first 0x20) [
        [:type-small-constant second]
        [:type-small-constant third]]
      (< first 0x40) [
        [:type-small-constant second]
        [:type-variable third]]
      (< first 0x60)  [
        [:type-variable second]
        [:type-small-constant third]]
      :else [
          [:type-variable second]
          [:type-variable third]])
    [store branch-offset] (parse-tail instruction tail)]
  {
    :name (:name instruction)
    :form :form-long
    :opcode first
    :operand-count :2OP
    :operands operands
    :store store
    :branch-offset branch-offset
    :text-to-print nil
    :byte-count (+ 3 (if store 1 0) (count (clojure.core/second branch-offset)))
  }))

(defn make-short-form [bytes]
  (let [
    [first second third fourth] bytes
    instruction (instruction-table first)
    operand-count (if (<= first 0xaf) :1OP :0OP)
    operands (cond
      (<= first 0x8f) [[:type-large-constant second third]]
      (<= first 0x9f) [[:type-small-constant second]]
      (<= first 0xaf) [[:type-variable second]]
      (<= first 0xbf) [])
    tail (get-tail 1 operands bytes)
    [store branch-offset] (parse-tail instruction tail)
    text2print-count (case first
      (0xb2 0xb3) (count-text2print-bytes (rest bytes))
      nil)
  ]
  {
    :name (:name instruction)
    :form :form-short
    :opcode first
    :operand-count operand-count
    :operands operands
    :store store
    :branch-offset branch-offset
    :text-to-print (case first
      (0xb2 0xb3) (text2print-decode (take-last text2print-count bytes))
      nil)
    :byte-count (case first
      (0xb2 0xb3) (+ 1 text2print-count)
      (+ 1 (count-operand-bytes operands) (if store 1 0) (count (clojure.core/second branch-offset))))
  }))

(defn make-variable-form [bytes]
  (let [
    [first second & rest] bytes
    instruction (instruction-table first)
    operand-types (decode-operand-types [second])
    operands (extract-operands operand-types rest)
    operand-count (if (<= first 0xdf) :2OP :VAR)
    tail (get-tail 2 operands bytes)
    [store branch-offset] (parse-tail instruction tail)
  ]
  {
    :name (:name instruction)
    :form :form-variable
    :opcode first
    :operand-count operand-count
    :operands operands
    :store store
    :branch-offset branch-offset
    :text-to-print nil
    :byte-count (+ 2 (count-operand-bytes operands) (if store 1 0) (count (clojure.core/second branch-offset)))
  }))

(defn make-variable-form-special [bytes]
  (let [
    [first second third & rest] bytes
    instruction (instruction-table first)
    operand-types (concat (decode-operand-types [second]) (decode-operand-types [third]))
    operands (extract-operands operand-types rest)
    store (if (= first 0xec) (last bytes) nil)
  ]
  {
    :name (:name instruction)
    :form :form-variable
    :opcode first
    :operand-count :VAR
    :operands operands
    :store store
    :branch-offset nil
    :text-to-print nil
    :byte-count (+ 3 (count-operand-bytes operands) (if store 1 0))
  }))

(defn make-extended-form [bytes]
  (let [
    [first second third & rest] bytes
    instruction (instruction-table-extended second)
    operand-types (decode-operand-types [third])
    operands (extract-operands operand-types rest)
    tail (get-tail 3 operands bytes)
    [store branch-offset] (parse-tail instruction tail)
  ] {
    :name (:name instruction)
    :form :form-extended
    :opcode second
    :operand-count :VAR
    :operands operands
    :store store
    :branch-offset branch-offset
    :text-to-print nil
    :byte-count (+ 3 (count-operand-bytes operands) (if store 1 0) (count (clojure.core/second branch-offset)))
  }))

(defn decode [bytes] 
  (let [[first second third fourth fifth sixth] bytes]
    (cond
      (= first 0xbe) (make-extended-form bytes)
      (or (= first 0xec) (= first 0xfa)) (make-variable-form-special bytes)
      (<= first 0x7f) (make-long-form bytes)
      (<= first 0xbf) (make-short-form bytes)
      (<= first 0xff) (make-variable-form bytes))))

(defn decode-all [bytes]
  (if (empty? bytes)
    []
    (let [
      instruction (decode bytes)
      next-bytes (drop (:byte-count instruction) bytes)
    ]
    (concat [instruction] (decode-all next-bytes)))
  ))

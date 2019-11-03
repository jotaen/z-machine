(ns z-machine.instruction
  (:require [z-machine.instruction-table :refer :all]))

(defn decode-operand-types [bytes]
  (defn optype [val]
    (case val
      2r00 :type-large-constant
      2r10 :type-variable
      2r01 :type-small-constant
      2r11 :type-omitted))
  (let [
    [first] bytes
    type1 (optype (bit-and (bit-shift-right first 6) 2r11))
    type2 (optype (bit-and (bit-shift-right first 4) 2r11))
    type3 (optype (bit-and (bit-shift-right first 2) 2r11))
    type4 (optype (bit-and (bit-shift-right first 0) 2r11))
    ]
    (filter (fn [x] (not= x :type-omitted)) [type1 type2 type3 type4])))

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
    [store branch-offset] (parse-tail instruction tail)]
  {
    :name (:name instruction)
    :form :form-short
    :opcode first
    :operand-count operand-count
    :operands operands
    :store store
    :branch-offset branch-offset
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
  }))

(defn make-variable-form-special [bytes]
  (let [
    [first second third & rest] bytes
    instruction (instruction-table first)
    operand-types (concat (decode-operand-types [second]) (decode-operand-types [third]))
    operands (extract-operands operand-types rest)
  ]
  {
    :name (:name instruction)
    :form :form-variable
    :opcode first
    :operand-count :VAR
    :operands operands
    :store (if (= first 0xec) (last bytes) nil)
    :branch-offset nil
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
  }))

(defn decode [bytes] 
  (let [[first second third fourth fifth sixth] bytes]
    (cond
      (= first 0xbe) (make-extended-form bytes)
      (or (= first 0xec) (= first 0xfa)) (make-variable-form-special bytes)
      (<= first 0x7f) (make-long-form bytes)
      (<= first 0xbf) (make-short-form bytes)
      (<= first 0xff) (make-variable-form bytes))))

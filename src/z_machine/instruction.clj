(ns z-machine.instruction
  (:require [z-machine.opcode-names :refer :all]))

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

(defn make-long-form [bytes]
  (let [
    [first second third fourth] bytes
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
          [:type-variable third]])]
  {
    :name (instruction-names first)
    :form :form-long
    :opcode first
    :operand-count :2OP
    :operands operands
    :branch-offset [fourth]
    :store nil}))

(defn make-short-form [bytes]
  (let [
    [first second third fourth] bytes
    operand-count (if (<= first 0xaf) :1OP :0OP)
    operands (cond
      (<= first 0x8f) [[:type-large-constant second third]]
      (<= first 0x9f) [[:type-small-constant second]]
      (<= first 0xaf) [[:type-variable second]]
      (<= first 0xbf) [])
    branch-offset (cond
      (<= first 0x8f) [fourth]
      (<= first 0xaf) [third]
      (<= first 0xbf) [second])]
  {
    :name (instruction-names first)
    :form :form-short
    :opcode first
    :operand-count operand-count
    :operands operands
    :branch-offset branch-offset
    :store nil}))

(defn make-variable-form [bytes]
  (let [
    [first second & rest] bytes
    operand-types (decode-operand-types [second])
    operands (extract-operands operand-types rest)
    operand-count (if (<= first 0xdf) :2OP :VAR)
  ]
  {
    :name (instruction-names first)
    :form :form-variable
    :opcode first
    :operand-count operand-count
    :operands operands
    :branch-offset nil
    :store (last bytes)
  }))

(defn make-variable-form-special [bytes]
  (let [
    [first second third & rest] bytes
    operand-types (concat (decode-operand-types [second]) (decode-operand-types [third]))
    operands (extract-operands operand-types rest)
  ]
  {
    :name (instruction-names first)
    :form :form-variable
    :opcode first
    :operand-count :VAR
    :operands operands
    :branch-offset nil
    :store (if (= first 0xec) (last bytes) nil)
  }))

(defn make-extended-form [bytes]
  (let [
    [first second third & rest] bytes
    operand-types (decode-operand-types [third])
    operands (extract-operands operand-types rest)
  ] {
    :name (instruction-names-extended second)
    :form :form-extended
    :opcode second
    :operand-count :VAR
    :operands operands
    :branch-offset nil
    :store (last bytes)
  }))

(defn decode [bytes] 
  (let [[first second third fourth fifth sixth] bytes]
    (cond
      (= first 0xbe) (make-extended-form bytes)
      (or (= first 0xec) (= first 0xfa)) (make-variable-form-special bytes)
      (<= first 0x7f) (make-long-form bytes)
      (<= first 0xbf) (make-short-form bytes)
      (<= first 0xff) (make-variable-form bytes))))

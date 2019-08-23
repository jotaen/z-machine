(ns z-machine.instruction)

(defn instruction-names [byte]
  (case byte
    (0x04 0x24 0x44 0x64) :dec_chk
    (0x05 0x25 0x45 0x65) :inc_chk
    (0x80 0x90 0xa0) :jz
    (0xb5) :save
    (0xd6) :mul
    (0xd5) :sub
  ))

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
    [first second third fourth fifth sixth] bytes
    operands [
      [:type-large-constant third fourth]
      [:type-variable fifth]
    ]
  ]
  {
    :name (instruction-names first)
    :form :form-variable
    :opcode first
    :operand-count :2OP
    :operands operands
    :branch-offset nil
    :store sixth
  }))

(defn decode [bytes] 
  (let [[first second third fourth fifth sixth] bytes]
    (cond
      (<= first 0x7f) (make-long-form bytes)
      (<= first 0xbf) (make-short-form bytes)
      (<= first 0xdf) (make-variable-form bytes))))

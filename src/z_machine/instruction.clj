(ns z-machine.instruction)

(defn instruction-names [byte]
  (case byte
    (0x04 0x24 0x44 0x64) :dec_chk
    (0x05 0x25 0x45 0x65) :inc_chk
    (0x80 0x90 0xa0) :jz
    (0xb5) :save
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
    :branch-offset [fourth]}))

(defn decode [bytes] 
  (let [[first second third fourth] bytes]
    (cond
      (<= first 0x7f) (make-long-form bytes)
      (<= first 0x8f) {
        :name (instruction-names first)
        :form :form-short
        :opcode first
        :operand-count :1OP
        :operands [[:type-large-constant second third]]
        :branch-offset [fourth]}
      (<= first 0x9f) {
        :name (instruction-names first)
        :form :form-short
        :opcode first
        :operand-count :1OP
        :operands [[:type-small-constant second]]
        :branch-offset [third]}
      (<= first 0xaf) {
        :name (instruction-names first)
        :form :form-short
        :opcode first
        :operand-count :1OP
        :operands [[:type-variable second]]
        :branch-offset [third]}
      (<= first 0xbf) {
        :name (instruction-names first)
        :form :form-short
        :opcode first
        :operand-count :0OP
        :operands []
        :branch-offset [second]})))

(ns z-machine.instruction)

(defn instruction-names [byte]
  (case byte
    (0x04 0x24 0x44 0x64) :dec_chk
    (0x05 0x25 0x45 0x65) :inc_chk
  ))

(defn operands-long-form [bytes]
  (let [[first second third] bytes]
    (cond
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
          [:type-variable third]])))

(defn decode [bytes] 
  (let [[first second third fourth] bytes]
    (cond
      (<= first 0x7f) {
        :name (instruction-names first)
        :form :form-long
        :opcode first
        :operand-count :2OP
        :operands (operands-long-form bytes)
        :branch-offset [fourth]})))

(ns z-machine.instruction)

(defn instruction-names [byte]
  (case byte
    (0x04 0x24) :dec_chk
    (0x05 0x25) :inc_chk
  ))

(defn decode [bytes] 
  (let [[first second third fourth] bytes]
    (cond
      (< first 0x20) {
        :name (instruction-names first)
        :form :form-long
        :opcode first
        :operand-count :2OP
        :operands [
          [:type-small-constant second]
          [:type-small-constant third]
        ]
        :branch-offset [fourth]}
      :else {
        :name (instruction-names first)
        :form :form-long
        :opcode first
        :operand-count :2OP
        :operands [
          [:type-small-constant second]
          [:type-variable third]
        ]
        :branch-offset [fourth]})))

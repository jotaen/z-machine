(ns z-machine.instruction-test
 (:require [clojure.test :refer :all]
           [z-machine.instruction :refer :all]) 
)

(def instructions {
  :inc_chk-SS [0x05 0x02 0x00 0xd4]
  :dec_chk-SS [0x04 0x12 0xab 0xd5]
  :inc_chk-SV [0x25 0x02 0x00 0xd4]
  :dec_chk-SV [0x24 0x21 0x1e 0xd6]
})

(deftest instruction-decoder
  (testing "range 0x00 to 0x1f"
    (is (= (decode (:inc_chk-SS instructions)) {
        :name :inc_chk
        :form :form-long
        :opcode 0x05
        :operand-count :2OP
        :operands [
          [:type-small-constant 0x02]
          [:type-small-constant 0x00]
        ]
        :branch-offset [0xd4]
      }))
    (is (= (decode (:dec_chk-SS instructions)) {
        :name :dec_chk
        :form :form-long
        :opcode 0x04
        :operand-count :2OP
        :operands [
          [:type-small-constant 0x12]
          [:type-small-constant 0xab]
        ]
        :branch-offset [0xd5]
      })))

  (testing "range 0x20 0x3f"
    (is (= (decode (:inc_chk-SV instructions)) {
        :name :inc_chk
        :form :form-long
        :opcode 0x25
        :operand-count :2OP
        :operands [
          [:type-small-constant 0x02]
          [:type-variable 0x00]
        ]
        :branch-offset [0xd4]
      }))
    (is (= (decode (:dec_chk-SV instructions)) {
        :name :dec_chk
        :form :form-long
        :opcode 0x24
        :operand-count :2OP
        :operands [
          [:type-small-constant 0x21]
          [:type-variable 0x1e]
        ]
        :branch-offset [0xd6]
      })))
)

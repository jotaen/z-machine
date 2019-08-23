(ns z-machine.instruction-test
 (:require [clojure.test :refer :all]
           [z-machine.instruction :refer :all]) 
)

(def instructions {
  :inc_chk-SS [0x05 0x02 0x00 0xd4]
  :dec_chk-SS [0x04 0x12 0xab 0xd5]
  :inc_chk-SV [0x25 0x02 0x00 0xd4]
  :dec_chk-SV [0x24 0x21 0x1e 0xd6]
  :inc_chk-VS [0x45 0x00 0x02 0xd6]
  :dec_chk-VS [0x44 0x1e 0x21 0xd4]
  :inc_chk-VV [0x65 0x10 0x32 0xd5]
  :dec_chk-VV [0x64 0x32 0x10 0xd7]
  :jz-L [0x80 0xdd 0x23 0xd3]
  :jz-S [0x90 0x1a 0xd3]
  :jz-V [0xa0 0x65 0xd3]
  :save [0xb5 0xd8]
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

  (testing "range 0x20 to 0x3f"
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

  (testing "range 0x40 to 0x5f"
    (is (= (decode (:inc_chk-VS instructions)) {
        :name :inc_chk
        :form :form-long
        :opcode 0x45
        :operand-count :2OP
        :operands [
          [:type-variable 0x00]
          [:type-small-constant 0x02]
        ]
        :branch-offset [0xd6]
      }))
    (is (= (decode (:dec_chk-VS instructions)) {
        :name :dec_chk
        :form :form-long
        :opcode 0x44
        :operand-count :2OP
        :operands [
          [:type-variable 0x1e]
          [:type-small-constant 0x21]
        ]
        :branch-offset [0xd4]
      })))

  (testing "range 0x60 to 0x7f"
    (is (= (decode (:inc_chk-VV instructions)) {
        :name :inc_chk
        :form :form-long
        :opcode 0x65
        :operand-count :2OP
        :operands [
          [:type-variable 0x10]
          [:type-variable 0x32]
        ]
        :branch-offset [0xd5]
      }))
    (is (= (decode (:dec_chk-VV instructions)) {
        :name :dec_chk
        :form :form-long
        :opcode 0x64
        :operand-count :2OP
        :operands [
          [:type-variable 0x32]
          [:type-variable 0x10]
        ]
        :branch-offset [0xd7]
      })))

  (testing "range 0x80 to 0x8f"
    (is (= (decode (:jz-L instructions)) {
        :name :jz
        :form :form-short
        :opcode 0x80
        :operand-count :1OP
        :operands [
          [:type-large-constant 0xdd 0x23]
        ]
        :branch-offset [0xd3]
      })))

  (testing "range 0x90 to 0x9f"
    (is (= (decode (:jz-S instructions)) {
        :name :jz
        :form :form-short
        :opcode 0x90
        :operand-count :1OP
        :operands [
          [:type-small-constant 0x1a]
        ]
        :branch-offset [0xd3]
      })))

  (testing "range 0xa0 to 0xaf"
    (is (= (decode (:jz-V instructions)) {
        :name :jz
        :form :form-short
        :opcode 0xa0
        :operand-count :1OP
        :operands [
          [:type-variable 0x65]
        ]
        :branch-offset [0xd3]
      })))

  (testing "range 0xb0 to 0xbf"
    (is (= (decode (:save instructions)) {
        :name :save
        :form :form-short
        :opcode 0xb5
        :operand-count :0OP
        :operands []
        :branch-offset [0xd8]
      })))

  )

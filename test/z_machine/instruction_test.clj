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
  :mul-LV [0xd6 0x2f 0x03 0xe8 0x02 0x00]
  :sub-LV [0xd5 0x2f 0x04 0xe9 0x03 0x01]
  :mul-VL [0xd6 0x8f 0x01 0x02 0x03 0x00]
  :mul-LL [0xd6 0x0f 0x03 0x04 0x05 0x06 0x00]
  :call [0xe0 0xff 0x01]
  :call-LLL [0xe0 0x03 0x01 0x02 0x03 0x04 0x05 0x06 0x09]
  :set_font [0xbe 0x04 0x3f 0x10 0xd2 0x08]
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
        :store nil
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
        :store nil
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
        :store nil
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
        :store nil
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
        :store nil
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
        :store nil
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
        :store nil
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
        :store nil
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
        :store nil
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
        :store nil
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
        :store nil
      })))

  (testing "range 0xb0 to 0xbf"
    (is (= (decode (:save instructions)) {
        :name :save
        :form :form-short
        :opcode 0xb5
        :operand-count :0OP
        :operands []
        :branch-offset [0xd8]
        :store nil
      })))

  (testing "range 0xc0 to 0xdf"
    (is (= (decode (:mul-LV instructions)) {
        :name :mul
        :form :form-variable
        :opcode 0xd6
        :operand-count :2OP
        :operands [
          [:type-large-constant 0x03 0xe8]
          [:type-variable 0x02]
        ]
        :branch-offset nil
        :store 0x00
      }))
    (is (= (decode (:mul-LL instructions)) {
        :name :mul
        :form :form-variable
        :opcode 0xd6
        :operand-count :2OP
        :operands [
          [:type-large-constant 0x03 0x04]
          [:type-large-constant 0x05 0x06]
        ]
        :branch-offset nil
        :store 0x00
      }))
    (is (= (decode (:sub-LV instructions)) {
        :name :sub
        :form :form-variable
        :opcode 0xd5
        :operand-count :2OP
        :operands [
          [:type-large-constant 0x04 0xe9]
          [:type-variable 0x03]
        ]
        :branch-offset nil
        :store 0x01
      }))

    (is (= (decode (:mul-VL instructions)) {
        :name :mul
        :form :form-variable
        :opcode 0xd6
        :operand-count :2OP
        :operands [
          [:type-variable 0x01]
          [:type-large-constant 0x02 0x03]
        ]
        :branch-offset nil
        :store 0x00
      })))

  (testing "range 0xe0 to 0xff"
    (is (= (decode (:call instructions)) {
      :name :call
      :form :form-variable
      :opcode 0xe0
      :operand-count :VAR
      :operands []
      :branch-offset nil
      :store 0x01
      }))
    (is (= (decode (:call-LLL instructions)) {
      :name :call
      :form :form-variable
      :opcode 0xe0
      :operand-count :VAR
      :operands [
        [:type-large-constant 0x01 0x02]
        [:type-large-constant 0x03 0x04]
        [:type-large-constant 0x05 0x06]
      ]
      :branch-offset nil
      :store 0x09
      })))

  (testing "range 0xbe"
    (is (= (decode (:set_font instructions)) {
      :name :set_font
      :form :form-extended
      :opcode 0x04
      :operand-count :1OP
      :operands [
        [:type-large-constant 0x10 0xd2]
      ]
      :branch-offset nil
      :store 0x08
      })))

(deftest operand-type-decoder
  (testing "single byte"
    (is (= (decode-operand-types [0x2f]) [:type-large-constant :type-variable]))
    (is (= (decode-operand-types [0x8f]) [:type-variable :type-large-constant]))
    (is (= (decode-operand-types [0x0b]) [
      :type-large-constant
      :type-large-constant
      :type-variable
    ]))
    (is (= (decode-operand-types [0x68]) [
      :type-small-constant
      :type-variable
      :type-variable
      :type-large-constant
    ])))))

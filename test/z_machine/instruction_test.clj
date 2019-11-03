(ns z-machine.instruction-test
  (:require [clojure.test :refer :all]
            [z-machine.instruction :refer :all]))

(def instructions {
  ; LONG FORM, 2OP
  :dec_chk-SS [0x04 0x12 0xab 0xb4 0x72]
  :inc_chk-SS [0x05 0x02 0x00 0x03 0x69]
  :and-SS [0x09 0x01 0x00 0x11]
  :insert_obj-SS [0x0e 0x54 0xdc]
  :dec_chk-SV [0x24 0x21 0x1e 0xd6]
  :inc_chk-SV [0x25 0x02 0x00 0xd4]
  :dec_chk-VS [0x44 0x1e 0x21 0xc0]
  :inc_chk-VS [0x45 0x00 0x02 0xd6]
  :dec_chk-VV [0x64 0x32 0x10 0xd7]
  :inc_chk-VV [0x65 0x10 0x32 0xd5]

  ; SHORT FORM, 1OP
  :jz-L [0x80 0xdd 0x23 0xd3]
  :get_sibling-L [0x81 0xbe 0x73 0x64 0x02 0xfe]
  :jz-S [0x90 0x1a 0x7f]
  :call_1s-S [0x98 0xa2 0xe7]
  :jz-V [0xa0 0x65 0xd3]
  :ret-V [0xab 0x77]

  ; SHORT FORM, 0OP
  :verify [0xbd 0xd8]

  ; VAR FORM, 2OP 
  :test_attr-LS [0xca 0x1f 0x03 0x04 0x01 0x04 0x32]
  :insert_obj-VL [0xce 0x8f 0xcc 0xa6 0x35]
  :sub-LV [0xd5 0x2f 0x04 0xe9 0x03 0x01]
  :mul-LV [0xd6 0x2f 0x03 0xe8 0x02 0x00]
  :mul-VL [0xd6 0x8f 0x01 0x02 0x03 0x00]
  :mul-LL [0xd6 0x0f 0x03 0x04 0x05 0x06 0x00]

  ; VAR FORM, VAROP
  :call_vs [0xe0 0xff]
  :call_vs-LLL [0xe0 0x03 0x01 0x02 0x03 0x04 0x05 0x06]
  :scan_table [0xf7 0xff 0x01 0xf3]
  ; special
  :call_vs2 [0xec 0x58 0x8b 0xc1 0x03 0x45 0xe3 0x6f 0x0e 0x22 0xf8 0x9a 0x02]
  :call_vn2 [0xfa 0x8b 0xff 0x45 0xe3 0x6f 0x0e]
  
  ; EXTENDED FORM
  :log_shift-SV [0xbe 0x02 0x6f 0xae 0x8c 0x09]
  :set_font-L [0xbe 0x04 0x3f 0x10 0xd2 0x08]
  :save_undo [0xbe 0x09 0xff 0x04]
  :push_stack [0xbe 0x18 0xff 0x06 0x5e]
  :check_unicode-LSLV [0xbe 0x0c 0x12 0x72 0x8a 0x7c 0x11 0x00 0x74]
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
        :store nil
        :branch-offset [false [0x03 0x69]]
        :byte-count 5
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
        :store nil
        :branch-offset [true [0x34 0x72]]
        :byte-count 5
      }))
    (is (= (decode (:and-SS instructions)) {
        :name :and
        :form :form-long
        :opcode 0x09
        :operand-count :2OP
        :operands [
          [:type-small-constant 0x01]
          [:type-small-constant 0x00]
        ]
        :store 0x11
        :branch-offset nil
        :byte-count 4
      }))
    (is (= (decode (:insert_obj-SS instructions)) {
        :name :insert_obj
        :form :form-long
        :opcode 0x0e
        :operand-count :2OP
        :operands [
          [:type-small-constant 0x54]
          [:type-small-constant 0xdc]
        ]
        :store nil
        :branch-offset nil
        :byte-count 3
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
        :store nil
        :branch-offset [true [0x14]]
        :byte-count 4
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
        :store nil
        :branch-offset [true [0x16]]
        :byte-count 4
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
        :store nil
        :branch-offset [true [0x16]]
        :byte-count 4
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
        :store nil
        :branch-offset [true [0x00]]
        :byte-count 4
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
        :store nil
        :branch-offset [true [0x15]]
        :byte-count 4
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
        :store nil
        :branch-offset [true [0x17]]
        :byte-count 4
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
        :store nil
        :branch-offset [true [0x13]]
        :byte-count 4
      }))
    (is (= (decode (:get_sibling-L instructions)) {
        :name :get_sibling
        :form :form-short
        :opcode 0x81
        :operand-count :1OP
        :operands [
          [:type-large-constant 0xbe 0x73]
        ]
        :store 0x64
        :branch-offset [false [0x02 0xfe]]
        :byte-count 6
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
        :store nil
        :branch-offset [false [0x3f]]
        :byte-count 3
      }))
    (is (= (decode (:call_1s-S instructions)) {
        :name :call_1s
        :form :form-short
        :opcode 0x98
        :operand-count :1OP
        :operands [
          [:type-small-constant 0xa2]
        ]
        :store 0xe7
        :branch-offset nil
        :byte-count 3
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
        :store nil
        :branch-offset [true [0x13]]
        :byte-count 3
      }))
    (is (= (decode (:ret-V instructions)) {
        :name :ret
        :form :form-short
        :opcode 0xab
        :operand-count :1OP
        :operands [
          [:type-variable 0x77]
        ]
        :store nil
        :branch-offset nil
        :byte-count 2
      })))

  (testing "range 0xb0 to 0xbf"
    (is (= (decode (:verify instructions)) {
        :name :verify
        :form :form-short
        :opcode 0xbd
        :operand-count :0OP
        :operands []
        :store nil
        :branch-offset [true [0x18]]
        :byte-count 2
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
        :store 0x00
        :branch-offset nil
        :byte-count 6
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
        :store 0x00
        :branch-offset nil
        :byte-count 7
      }))
    (is (= (decode (:test_attr-LS instructions)) {
        :name :test_attr
        :form :form-variable
        :opcode 0xca
        :operand-count :2OP
        :operands [
          [:type-large-constant 0x03 0x04]
          [:type-small-constant 0x01]
        ]
        :store nil
        :branch-offset [false [0x04 0x32]]
        :byte-count 7
      }))
    (is (= (decode (:insert_obj-VL instructions)) {
        :name :insert_obj
        :form :form-variable
        :opcode 0xce
        :operand-count :2OP
        :operands [
          [:type-variable 0xcc]
          [:type-large-constant 0xa6 0x35]
        ]
        :store nil
        :branch-offset nil
        :byte-count 5
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
        :store 0x01
        :branch-offset nil
        :byte-count 6
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
        :store 0x00
        :branch-offset nil
        :byte-count 6
      })))

  (testing "range 0xe0 to 0xff"
    (is (= (decode (:call_vs instructions)) {
      :name :call_vs
      :form :form-variable
      :opcode 0xe0
      :operand-count :VAR
      :operands []
      :store nil
      :branch-offset nil
      :byte-count 2
      }))
    (is (= (decode (:call_vs-LLL instructions)) {
      :name :call_vs
      :form :form-variable
      :opcode 0xe0
      :operand-count :VAR
      :operands [
        [:type-large-constant 0x01 0x02]
        [:type-large-constant 0x03 0x04]
        [:type-large-constant 0x05 0x06]
      ]
      :store nil
      :branch-offset nil
      :byte-count 8
      }))
    (is (= (decode (:scan_table instructions)) {
      :name :scan_table
      :form :form-variable
      :opcode 0xf7
      :operand-count :VAR
      :operands []
      :store 0x01
      :branch-offset [true [0x33]]
      :byte-count 4
      })))

  (testing "special cases: 2-byte operand types"
    (is (= (decode (:call_vs2 instructions)) {
      :name :call_vs2
      :form :form-variable
      :opcode 0xec
      :operand-count :VAR
      :operands [
        [:type-small-constant 0xc1]
        [:type-small-constant 0x03]
        [:type-variable 0x45]
        [:type-large-constant 0xe3 0x6f]
        [:type-variable 0x0e]
        [:type-large-constant 0x22 0xf8]
        [:type-variable 0x9a]
      ]
      :store 0x02
      :branch-offset nil
      :byte-count 13
    }))
    (is (= (decode (:call_vn2 instructions)) {
      :name :call_vn2
      :form :form-variable
      :opcode 0xfa
      :operand-count :VAR
      :operands [
        [:type-variable 0x45]
        [:type-large-constant 0xe3 0x6f]
        [:type-variable 0x0e]
      ]
      :store nil
      :branch-offset nil
      :byte-count 7
    })))

  (testing "range 0xbe"
    (is (= (decode (:set_font-L instructions)) {
      :name :set_font
      :form :form-extended
      :opcode 0x04
      :operand-count :VAR
      :operands [
        [:type-large-constant 0x10 0xd2]
      ]
      :store 0x08
      :branch-offset nil
      :byte-count 6
      }))
    (is (= (decode (:save_undo instructions)) {
      :name :save_undo
      :form :form-extended
      :opcode 0x09
      :operand-count :VAR
      :operands []
      :store 0x04
      :branch-offset nil
      :byte-count 4
      }))
    (is (= (decode (:push_stack instructions)) {
      :name :push_stack
      :form :form-extended
      :opcode 0x18
      :operand-count :VAR
      :operands []
      :store nil
      :branch-offset [false [0x06 0x5e]]
      :byte-count 5
      }))
    (is (= (decode (:check_unicode-LSLV instructions)) {
      :name :check_unicode
      :form :form-extended
      :opcode 0x0c
      :operand-count :VAR
      :operands [
        [:type-large-constant 0x72 0x8a]
        [:type-small-constant 0x7c]
        [:type-large-constant 0x11 0x00]
        [:type-variable 0x74]
      ]
      :store nil
      :branch-offset nil
      :byte-count 9
      }))
    (is (= (decode (:log_shift-SV instructions)) {
      :name :log_shift
      :form :form-extended
      :opcode 0x02
      :operand-count :VAR
      :operands [
        [:type-small-constant 0xae]
        [:type-variable 0x8c]
      ]
      :store 0x09
      :branch-offset nil
      :byte-count 6
      })))

(deftest operand-byte-counter
  (testing "different operands"
    (is (= (count-operand-bytes []) 0))
    (is (= (count-operand-bytes [
        [:type-small-constant 0xae]
        [:type-variable 0x8c]
      ]) 2))
    (is (= (count-operand-bytes [
        [:type-large-constant 0x10 0xd2]
      ]) 2))
    (is (= (count-operand-bytes [
        [:type-small-constant 0xc1]
        [:type-small-constant 0x03]
        [:type-variable 0x45]
        [:type-large-constant 0xe3 0x6f]
        [:type-variable 0x0e]
        [:type-large-constant 0x22 0xf8]
        [:type-variable 0x9a]
      ]) 9))))

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
    ]))))

(deftest branch-decoder
  (testing "branch-offset with single byte"
    (is (= (decode-branch [0xfc]) [true [0x3c]]))
    (is (= (decode-branch [0x7c]) [false [0x3c]]))
    (is (= (decode-branch [0x58]) [false [0x18]]))
    )
  (testing "branch-offset with two bytes"
    (is (= (decode-branch [0xaa 0x29]) [true [0x2a 0x29]]))
    (is (= (decode-branch [0x3a 0x5f]) [false [0x3a 0x5f]]))
    ))

(deftest text2print-bytes-counter
  (testing "text to print with one word"
    (is (= (count-text2print-bytes [0xa4 0x37]) 2))
    (is (= (count-text2print-bytes [0x00 0x00 0xa4 0x37]) 4))
    (is (= (count-text2print-bytes [0x07 0x08 0x35 0x00 0xa4 0x37]) 6)))))

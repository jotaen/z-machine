(ns z-machine.core-test
  (:require [clojure.test :refer :all]
            [z-machine.opcode :refer :all]))

(deftest test-bits-extract
  (testing "it all"
    (is (= (bits-extract 2r01000000 0 2) 2r00000001))
    (is (= (bits-extract 2r01001010 2 5) 2r00000101))
    (is (= (bits-extract 2r01001010 0 8) 2r01001010))
    (is (= (bits-extract 2r01001010 0 0) 2r00000000))
    (is (= (bits-extract 2r10101010 4 2) 2r00000010))))

(deftest test-get-form
  (testing "`form-short`"
    (is (= (get-form 2r10000000) :form-short))
    (is (= (get-form 2r10000111) :form-short))
    (is (= (get-form 2r10100000) :form-short)))
  (testing "`form-long`"
    (is (= (get-form 2r01011110) :form-long))
    (is (= (get-form 2r00010111) :form-long))
    (is (= (get-form 2r00001000) :form-long)))
  (testing "`form-variable`"
    (is (= (get-form 2r11000000) :form-variable))
    (is (= (get-form 2r11001100) :form-variable))
    (is (= (get-form 2r11111111) :form-variable)))
  (testing "`form-extended`"
    (is (= (get-form 190) :form-extended))))

(deftest test-get-operand-count
  (testing "`form-short`"
    (is (= (get-operand-count 2r10011000) :0OP))
    (is (= (get-operand-count 2r10000000) :1OP))
    (is (= (get-operand-count 2r10001000) :1OP))
    (is (= (get-operand-count 2r10010000) :1OP)))
  (testing "`form-long`"
    (is (= (get-operand-count 2r01011110) :2OP)))
  (testing "`form-variable`"
    (is (= (get-operand-count 2r11001000) :VAROPS))
    (is (= (get-operand-count 2r11000000) :2OP)))
  (testing "`form-extended`"
    (is (= (get-operand-count 190) :VAROPS))))

(deftest test-get-opcode-number
  (testing "`form-short`"
    (is (= (get-opcode-number [2r10010101]) 5))
    (is (= (get-opcode-number [2r10011000]) 8))
    (is (= (get-opcode-number [2r10011001]) 9)))
  (testing "`form-long`"
    (is (= (get-opcode-number [2r01000000]) 0))
    (is (= (get-opcode-number [2r01011010]) 26))
    (is (= (get-opcode-number [2r01011110]) 30)))
  (testing "`form-variable`"
    (is (= (get-opcode-number [2r11001000]) 8))
    (is (= (get-opcode-number [2r11001011]) 11)))
  (testing "`form-extended`"
    (is (= (get-opcode-number [190 7]) 7))))

(deftest test-get-operand-types
  (testing "`form-short`"
    (is (= (get-operand-types [2r10000000]) [:type-large-constant]))
    (is (= (get-operand-types [2r10001000]) [:type-small-constant]))
    (is (= (get-operand-types [2r10010000]) [:type-a-variable]))
    (is (= (get-operand-types [2r10011000]) [:type-omitted])))
  (testing "`form-long`"
    (is (= (get-operand-types [2r01000000]) [:type-small-constant :type-small-constant]))
    (is (= (get-operand-types [2r01000100]) [:type-a-variable :type-small-constant]))
    (is (= (get-operand-types [2r01001000]) [:type-small-constant :type-a-variable]))
    (is (= (get-operand-types [2r01001100]) [:type-a-variable :type-a-variable])))
  (testing "`form-variable`"
    (is (= (get-operand-types [2r11001000 2r11011000]) [:type-large-constant :type-a-variable :type-small-constant :type-omitted]))
    (is (= (get-operand-types [2r11001000 2r11111101]) [:type-small-constant :type-omitted :type-omitted :type-omitted])))
  (testing "`form-extended`"
    (is (= (get-operand-types [190 2r11111100 2r11011000]) [:type-large-constant :type-a-variable :type-small-constant :type-omitted]))))
  ;; TODO assert that ommited must be be followed by type-omitted
  ;; TODO handle special case mentioned in 4.4.3.1 (double variable VAR opcodes)


(deftest test-get-operands
  (testing "`0OP`"
    (is (= (get-operands [2r10011000]) [])))
  (testing "`1OP`"
    (is (= (get-operands [2r10001000 2r10000001]) [129]))))

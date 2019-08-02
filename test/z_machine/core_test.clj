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
  (testing "`short`"
    (is (= (get-form 2r10000000) :short))
    (is (= (get-form 2r10000111) :short))
    (is (= (get-form 2r10100000) :short)))
  (testing "`long`"
    (is (= (get-form 2r01011110) :long))
    (is (= (get-form 2r00010111) :long))
    (is (= (get-form 2r00001000) :long)))
  (testing "`variable`"
    (is (= (get-form 2r11000000) :variable))
    (is (= (get-form 2r11001100) :variable))
    (is (= (get-form 2r11111111) :variable)))
  (testing "`extended`"
    (is (= (get-form 190) :extended))))

(deftest test-get-operand-count
  (testing "`short`"
    (is (= (get-operand-count 2r10011000) :0OP))
    (is (= (get-operand-count 2r10000000) :1OP))
    (is (= (get-operand-count 2r10001000) :1OP))
    (is (= (get-operand-count 2r10010000) :1OP)))
  (testing "`long`"
    (is (= (get-operand-count 2r01011110) :2OP)))
  (testing "`variable`"
    (is (= (get-operand-count 2r11001000) :VAR))
    (is (= (get-operand-count 2r11000000) :2OP)))
  (testing "`extended`"
    (is (= (get-operand-count 190) :VAR))))

(deftest test-get-opcode-number
  (testing "`short`"
    (is (= (get-opcode-number [2r10010101]) 5))
    (is (= (get-opcode-number [2r10011000]) 8))
    (is (= (get-opcode-number [2r10011001]) 9)))
  (testing "`long`"
    (is (= (get-opcode-number [2r01000000]) 0))
    (is (= (get-opcode-number [2r01011010]) 26))
    (is (= (get-opcode-number [2r01011110]) 30)))
  (testing "`variable`"
    (is (= (get-opcode-number [2r11001000]) 8))
    (is (= (get-opcode-number [2r11001011]) 11)))
  (testing "`extended`"
    (is (= (get-opcode-number [190 7]) 7))))

(deftest test-get-operand-types
  (testing "`short`"
    (is (= (get-operand-types [2r10000000]) [:large-constant]))
    (is (= (get-operand-types [2r10001000]) [:small-constant]))
    (is (= (get-operand-types [2r10010000]) [:variable]))
    (is (= (get-operand-types [2r10011000]) [:omitted])))
  (testing "`long`"
    (is (= (get-operand-types [2r01000000]) [:small-constant :small-constant]))
    (is (= (get-operand-types [2r01000100]) [:variable :small-constant]))
    (is (= (get-operand-types [2r01001000]) [:small-constant :variable]))
    (is (= (get-operand-types [2r01001100]) [:variable :variable])))
  (testing "`variable`"
    (is (= (get-operand-types [2r11001000 2r11011000]) [:large-constant :variable :small-constant :omitted]))
    (is (= (get-operand-types [2r11001000 2r11111101]) [:small-constant :omitted :omitted :omitted])))
  (testing "`extended`"
    (is (= (get-operand-types [190 2r11111100 2r11011000]) [:large-constant :variable :small-constant :omitted]))))
  ;; TODO assert that ommited must be be followed by omitted
  ;; TODO handle special case mentioned in 4.4.3.1 (double variable VAR opcodes)

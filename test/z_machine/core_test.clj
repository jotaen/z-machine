(ns z-machine.core-test
  (:require [clojure.test :refer :all]
            [z-machine.opcode :refer :all]))

(deftest test-get-form
  (testing "`short`"
    (is (= (get-form 2r10000000) :short))
    (is (= (get-form 2r10000111) :short))
    (is (= (get-form 2r10100000) :short)))
  (testing "`long`"
    (is (= (get-form 2r01011110) :long))
    (is (= (get-form 2r01010111) :long))
    (is (= (get-form 2r01000000) :long)))
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

(ns z-machine.core-test
  (:require [clojure.test :refer :all]
            [z-machine.opcode :refer :all]))

(deftest a-test
  (testing "`extended`"
    (is (= (get-form 190) :extended)))
  (testing "`short`"
    (is (= (get-form 2r10000000) :short))
    (is (= (get-form 2r10000111) :short))
    (is (= (get-form 2r10100000) :short)))
  (testing "`variable`"
    (is (= (get-form 2r11000000) :variable))
    (is (= (get-form 2r11001100) :variable))
    (is (= (get-form 2r11111111) :variable)))
  (testing "`long`"
    (is (= (get-form 2r01011110) :long))
    (is (= (get-form 2r01010111) :long))
    (is (= (get-form 2r01000000) :long)))
  )

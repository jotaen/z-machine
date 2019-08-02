(ns z-machine.core-test
  (:require [clojure.test :refer :all]
            [z-machine.core :refer :all]))

(defn extract-first-two-bits [b]
  (bit-and b 2r11000000))

(defn get-form [byte]
  (cond
    (= byte 190) :extended
    (= (extract-first-two-bits byte) 2r11000000) :variable
    (= (extract-first-two-bits byte) 2r10000000) :short
    :else :long))

(deftest a-test
  (testing "`extended`"
    (is (= (get-form 190) :extended)))
  (testing "`short`"
    (is (= (get-form 2r10000000) :short)))
  (testing "`variable`"
    (is (= (get-form 2r11000000) :variable)))
  (testing "`long`"
    (is (= (get-form 2r01010001) :long)))
  )

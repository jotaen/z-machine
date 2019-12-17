(ns z-machine.parser-test
  (:require [clojure.test :refer :all]
            [z-machine.parser :refer :all]))

(defn symbol-parser-a [str] 
  (let [first-char (first str)]
    (if (= first-char \a)
      [[:a (subs str 1)]]
      [[nil str]])))

(defn symbol-parser-b [str] 
  (let [first-char (first str)]
    (if (= first-char \b)
      [[:b (subs str 1)]]
      [[nil str]])))

(defn symbol-parser-c [str] 
  (let [first-char (first str)]
    (if (= first-char \c)
      [[:c (subs str 1)]]
      [[nil str]])))

(def double-a-parser (sequence-parser symbol-parser-a symbol-parser-a))
(def triple-a-parser (sequence-parser double-a-parser symbol-parser-a))
(def enum-ab-parser (enumeration-parser symbol-parser-a symbol-parser-b))
(def enum-abc-parser (enumeration-parser enum-ab-parser symbol-parser-c))

(deftest parse-test
  (testing "parse"
    (is (= (parse symbol-parser-a "a") [:a]))
    (is (= (parse symbol-parser-a "c") [nil]))
    (is (= (parse double-a-parser "aa") [:a :a]))
  ))

(deftest parser-test
  (testing "`a` character parser"
    (is (= (symbol-parser-a "ab") [[:a "b"]]))
    (is (= (symbol-parser-a "bb") [[nil "bb"]]))
  )
  (testing "sequence parser"
    (is (= (double-a-parser "aa") [[:a "a"] [:a ""]]))
    (is (= (double-a-parser "ab") [[:a "b"] [nil "b"]]))
    (is (= (double-a-parser "ba") [[nil "ba"]]))
    (is (= (double-a-parser "bb") [[nil "bb"]]))
    (is (= (triple-a-parser "aaa") [[:a "aa"] [:a "a"] [:a ""]]))
    (testing "associativity"
      (def p1 (sequence-parser double-a-parser symbol-parser-a))
      (def p2 (sequence-parser symbol-parser-a double-a-parser))
      (is (= (p1 "aaa") (p2 "aaa")))
    )
  )
  (testing "enumeration parser"
    (is (= (enum-ab-parser "a") [[:a ""]]))
    (is (= (enum-ab-parser "b") [[:b ""]]))
    (is (= (enum-ab-parser "c") [[nil "c"]]))
    (is (= (enum-abc-parser "c") [[:c ""]]))
))

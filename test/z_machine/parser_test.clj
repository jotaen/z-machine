(ns z-machine.parser-test
  (:require [clojure.test :refer :all]
            [z-machine.parser :refer :all]))

(defn make-symbol-parser [char symbol]
  (fn [str]
    (let [first-char (first str)]
      (if (= first-char char)
        [[symbol (subs str 1)]]
        [[nil str]]))))

(def symbol-parser-a (make-symbol-parser \a :a))
(def symbol-parser-b (make-symbol-parser \b :b))
(def symbol-parser-c (make-symbol-parser \c :c))

(def double-a-parser (sequence-parser symbol-parser-a symbol-parser-a))
(def triple-a-parser (sequence-parser double-a-parser symbol-parser-a))
(def enum-ab-parser (enumeration-parser symbol-parser-a symbol-parser-b))
(def enum-abc-parser (enumeration-parser enum-ab-parser symbol-parser-c))
   
(deftest parse-test
  (testing "parse"
    (is (= (parse symbol-parser-a "a") [:a]))
    (is (= (parse symbol-parser-a "c") [nil]))
    (is (= (parse double-a-parser "aa") [:a :a]))
  )
  (testing "integration"
    (def hello-parser
      (->                (make-symbol-parser \h :h)
        (sequence-parser (make-symbol-parser \e :e))
        (sequence-parser (make-symbol-parser \l :l))
        (sequence-parser (make-symbol-parser \l :l))
        (sequence-parser (make-symbol-parser \o :o))
        )
    )
    (def world-parser
      (->                (make-symbol-parser \w :w)
        (sequence-parser (make-symbol-parser \o :o))
        (sequence-parser (make-symbol-parser \r :r))
        (sequence-parser (make-symbol-parser \l :l))
        (sequence-parser (make-symbol-parser \d :d))
        )
    )
    (def integration-parser (enumeration-parser hello-parser world-parser))
    (is (= (parse integration-parser "hello") [:h :e :l :l :o]))
    (is (= (parse integration-parser "world") [:w :o :r :l :d]))
    (is (= (parse integration-parser "w812763") [:w nil]))
    (is (= (parse integration-parser "worl812763") [:w :o :r :l nil]))
    (is (= (parse integration-parser "a") [nil]))
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
    (is (= (double-a-parser "a1") [[:a "1"] [nil "1"]]))
    (is (= (double-a-parser "a12") [[:a "12"] [nil "12"]]))
    (is (= (triple-a-parser "a1a") [[:a "1a"] [nil "1a"]]))
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

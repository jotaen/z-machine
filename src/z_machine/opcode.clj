(ns z-machine.opcode)

(defn extract-first-two-bits [b]
  (bit-and b 2r11000000))

(defn get-form [byte]
  (cond
    (= byte 190) :extended
    (= (extract-first-two-bits byte) 2r11000000) :variable
    (= (extract-first-two-bits byte) 2r10000000) :short
    :else :long))

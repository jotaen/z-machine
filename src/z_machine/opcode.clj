(ns z-machine.opcode)

(defn extract-first-two-bits [b]
  (bit-and b 2r11000000))

(defn extract-fifth-bit [b]
  (bit-and b 2r00001000))

(defn extract-fourth-and-fifth-bit [b]
  (bit-and b 2r00011000))

(defn get-form [byte]
  (cond
    (= byte 190) :extended
    (= (extract-first-two-bits byte) 2r11000000) :variable
    (= (extract-first-two-bits byte) 2r10000000) :short
    :else :long))

(defn get-operand-count [byte]
  (case (get-form byte)
    :variable (if (= (extract-fifth-bit byte) 2r00001000) :VAR :2OP)
    :short (if (= (extract-fourth-and-fifth-bit byte) 2r00011000) :0OP :1OP)
    :extended :VAR
    :long :2OP))

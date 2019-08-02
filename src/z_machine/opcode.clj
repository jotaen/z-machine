(ns z-machine.opcode)

(defn bits-extract [byte start len]
  (bit-shift-right (bit-and (bit-shift-left byte start) 2r11111111) (- 8 len)))

(defn get-form [byte]
  (cond
    (= byte 190) :extended
    (= (bits-extract byte 0 2) 2r11) :variable
    (= (bits-extract byte 0 2) 2r10) :short
    :else :long))

(defn get-operand-count [byte]
  (case (get-form byte)
    :variable (if (= (bits-extract byte 4 1) 2r1) :VAR :2OP)
    :short (if (= (bits-extract byte 3 2) 2r11) :0OP :1OP)
    :extended :VAR
    :long :2OP))

(defn get-opcode-number [bytes]
  (let [[b1 b2] bytes]
    (case (get-form b1)
      :short (bits-extract b1 4 4)
      :long (bits-extract b1 3 5)
      :variable (bits-extract b1 3 5)
      :extended b2)))

(defn get-operand-types [bytes]
  (defn two-digit-type [byte]
    (case byte
      2r00 :large-constant
      2r01 :small-constant
      2r10 :variable
      2r11 :omitted))
  (defn one-digit-type [byte]
    (case byte
      2r0 :small-constant
      2r1 :variable))
  (let [[b1 b2 b3] bytes]
    (case (get-form b1)
      :short [
        (two-digit-type (bits-extract b1 3 2))]
      :long [
        (one-digit-type (bits-extract b1 5 1))
        (one-digit-type (bits-extract b1 4 1))]
      :variable [
        (two-digit-type (bits-extract b2 6 2))
        (two-digit-type (bits-extract b2 4 2))
        (two-digit-type (bits-extract b2 2 2))
        (two-digit-type (bits-extract b2 0 2))
      ]
      :extended [
        ; in extended form b2 is used for the opcode number
        (two-digit-type (bits-extract b3 6 2))
        (two-digit-type (bits-extract b3 4 2))
        (two-digit-type (bits-extract b3 2 2))
        (two-digit-type (bits-extract b3 0 2)) 
      ])))

(defn get-operands [bytes]
  (case (get-operand-count (first bytes))
      :0OP []
      :1OP [(second bytes)]))

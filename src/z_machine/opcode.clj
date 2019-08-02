(ns z-machine.opcode)

(defn bits-extract [byte start-from-left len]
  (bit-shift-right (bit-and (bit-shift-left byte start-from-left) 2r11111111) (- 8 len)))

(defn get-form [byte]
  (cond
    (= byte 190) :form-extended
    (= (bits-extract byte 0 2) 2r11) :form-variable
    (= (bits-extract byte 0 2) 2r10) :form-short
    :else :form-long))

(defn get-operand-count [byte]
  (case (get-form byte)
    :form-short (if (= (bits-extract byte 2 2) 2r11) :0OP :1OP)
    :form-long :2OP
    :form-variable (if (= (bits-extract byte 2 1) 2r1) :VAROPS :2OP)
    :form-extended :VAROPS))

(defn get-opcode-number [bytes]
  (let [[b1 b2] bytes]
    (case (get-form b1)
      :form-short (bits-extract b1 4 4)
      :form-long (bits-extract b1 3 5)
      :form-variable (bits-extract b1 3 5)
      :form-extended b2)))

(defn get-operand-types [bytes]
  (defn two-digit-type [byte]
    (case byte
      2r00 :type-large-constant
      2r01 :type-small-constant
      2r10 :type-a-variable
      2r11 :type-omitted))
  (defn one-digit-type [byte]
    (case byte
      2r0 :type-small-constant
      2r1 :type-a-variable))
  (let [[b1 b2 b3] bytes]
    (case (get-form b1)
      :form-short [
        (two-digit-type (bits-extract b1 2 2))]
      :form-long [
        (one-digit-type (bits-extract b1 1 1))
        (one-digit-type (bits-extract b1 2 1))]
      :form-variable [
        (two-digit-type (bits-extract b2 0 2))
        (two-digit-type (bits-extract b2 2 2))
        (two-digit-type (bits-extract b2 4 2))
        (two-digit-type (bits-extract b2 6 2))
      ]
      :form-extended [
        ; in form-extended form b2 is used for the opcode number
        (two-digit-type (bits-extract b3 0 2))
        (two-digit-type (bits-extract b3 2 2))
        (two-digit-type (bits-extract b3 4 2))
        (two-digit-type (bits-extract b3 6 2)) 
      ])))

(defn get-operands [bytes]
  (case (get-operand-count (first bytes))
      :0OP []
      :1OP [(second bytes)]))

(ns z-machine.parser)

(defn parse [parser str] (map first (parser str)))

(defn sequence-parser [p1 p2]
  (fn [str]
    (let [[val1 rst1] (first (p1 str))]
      (if (nil? val1)
        [[nil str]]
        (let [[val2 rst2] (first (p2 rst1))]
          [[val1 rst1] [val2 rst2]])))))

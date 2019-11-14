(ns z-machine.parser)

(defn parse [parser str] (map first (parser str)))

(def error-token nil)
(def error-token? nil?)

(defn sequence-parser [p1 p2]
  (defn and-then-concat [next-fn step]
    (let [[token remainder] (last step)]
      (if (error-token? token)
          [[error-token remainder]]
          (vec (concat step (next-fn remainder)))
  )))
  (fn [str]
    (->> (p1 str)
      (and-then-concat (fn [rst1] (p2 rst1)))
  )
))

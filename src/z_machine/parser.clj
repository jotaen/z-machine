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

(defn enumeration-parser [p1 p2]
  (fn [str]
    (let [
      result-a (p1 str)
      result-b (p2 str)
    ]
    (if (error-token? (first (first result-a)))
      result-b
      result-a)) 
  ))

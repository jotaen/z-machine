(ns z-machine.parser)

(defn parse [parser input] (map first (parser input)))

(def error-token nil)
(def error-token? nil?)

(defn and-parser [p1 p2]
  (defn next-unless-error [next-fn steps]
    (let [[token remainder] (last steps)]
      (if (error-token? token)
          steps
          (vec (concat steps (next-fn remainder)))
  )))
  (fn [input] (next-unless-error p2 (p1 input))
))

(defn or-parser [p1 p2]
  (fn [input]
    (let [
      result-a (p1 input)
      result-b (p2 input)
    ]
    (if (error-token? (first (first result-a)))
      result-b
      result-a)) 
  ))

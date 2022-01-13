(def positions (->> (slurp "d7.txt")
                    (#(str "[" % "]"))
                    (read-string)))

(def min-pos (apply min positions))
(def max-pos (apply max positions))

(defn abs-difference [x y] (Math/abs (- x y)))
(defn triangular-number [n] (quot (* n (inc n)) 2))

(defn dist-sum [dist-fn reference]
  (apply + (map #(dist-fn % reference) positions)))

(defn solve [dist-fn]
  (loop [left min-pos
         right max-pos]
    (let [mid (quot (+ left right) 2)
          [prev-cost mid-cost succ-cost] (map (partial dist-sum dist-fn) [(dec mid) mid (inc mid)])
          lowest-cost (min prev-cost mid-cost succ-cost)]
      (condp = lowest-cost
        prev-cost (recur left (dec mid))
        succ-cost (recur (inc mid) right)
        mid-cost))))

; part 1
(solve abs-difference)

; part 2
(solve (comp triangular-number abs-difference))

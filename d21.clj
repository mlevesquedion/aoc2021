(def p1-init-pos 8)
(def p2-init-pos 5)

(def die-rolls (map (partial apply +) (partition 3 (cycle (range 1 101)))))

(defn move [pos roll]
  (inc (mod (dec (+ pos roll)) 10)))

; part 1
(loop [p1-pos p1-init-pos, p1-total 0
       p2-pos p2-init-pos, p2-total 0
       [roll & more] die-rolls
       die-roll-count 0]
  (cond (>= p1-total 1000) (* p2-total die-roll-count)
        (>= p2-total 1000) (* p1-total die-roll-count)
        :else
        (let [p1-pos (move p1-pos roll)
              p1-total (+ p1-total p1-pos)]
          (recur p2-pos p2-total p1-pos p1-total more (+ 3 die-roll-count)))))

(def dirac-values (range 1 4))

(defn roll-dirac [xs]
  (if (nil? xs) dirac-values
      (for [dv dirac-values
            x xs]
        (+ dv x))))

(def dirac-rolls (frequencies (nth (iterate roll-dirac nil) 3)))

(def next-turn {:p1 :p2 :p2 :p1})

(defn solve [p1-score p1-pos p2-score p2-pos whose-turn]
  (if (>= p2-score 21)
    (case whose-turn :p1 [0 1] :p2 [1 0])
    (reduce
     (partial map +)
     [0 0]
     (for [[roll roll-count] dirac-rolls]
       (let [next-pos (move p1-pos roll)]
         (map (partial * roll-count) (solve p2-score p2-pos
                                            (+ p1-score next-pos) next-pos
                                            (next-turn whose-turn))))))))
(def solve (memoize solve))

; part 2
(apply max (solve 0 p1-init-pos 0 p2-init-pos :p1))

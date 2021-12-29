(defn simulate [x-vel y-vel min-x max-x min-y max-y]
  (loop
   [x 0, y 0
    x-vel x-vel, y-vel y-vel
    in-target false
    highest-y 0]
    (if (or (< y min-y) (> x max-x)) [in-target highest-y]
        (let [new-x (+ x x-vel)
              new-y (+ y y-vel)]
          (recur
           new-x, new-y
           (max 0 (dec x-vel)), (dec y-vel)
           (or in-target (and (<= min-x x max-x) (<= min-y y max-y)))
           (max new-y highest-y))))))

(defn simulate-reasonable-ranges [min-x max-x min-y max-y]
  (for [x-vel (range (inc max-x)) y-vel (range min-y (inc (- min-y)))]
    (simulate x-vel y-vel min-x max-x min-y max-y)))

(def successful-simulations (filter first (simulate-reasonable-ranges 94 151 -156 -103)))

; part 1
(apply max (map second successful-simulations))

; part 2
(count successful-simulations)

(def input
  (->> (slurp "d2.txt")
       (clojure.string/split-lines)
       (map #(clojure.string/split % #" "))
       (map (fn [[instr n-str]] [instr (Integer/parseInt n-str)]))))

(defn pos-delta [[instr n]]
  (case instr
    "forward" [n 0]
    "up" [0 (- n)]
    "down" [0 n]))

(->> input
     (map pos-delta)
     (apply (partial map vector)) ; transpose
     (map (partial apply +))
     (apply *))

(defn apply-instr [instr n horiz depth aim]
  (case instr
    "forward" [(+ horiz n) (+ depth (* aim n)) aim]
    "down" [horiz depth (+ aim n)]
    "up" [horiz depth (- aim n)]))

(->> input
     (reduce (fn [[horiz depth aim] [instr n]] (apply-instr instr n horiz depth aim)) [0 0 0])
     (take 2)
     (apply *))

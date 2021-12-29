(def points (->>
             (slurp "d5.txt")
             clojure.string/split-lines
             (map #(re-seq #"\d+" %))
             (map (partial map #(Integer/parseInt %)))))

(def straight-line-points
  (filter (fn [[x1 y1 x2 y2]] (or (= x1 x2) (= y1 y2))) points))

(def max-dim
  (inc (apply max (flatten points))))

(def diagram
  (vec (repeat max-dim (vec (repeat max-dim 0)))))

(defn line-values [from to]
  (cond (< from to) (range from (inc to))
        (< to from) (reverse (line-values to from))
        :else (repeat from)))

(defn line-segment [[x1 y1 x2 y2]]
  (map vector (line-values x1 x2) (line-values y1 y2)))

(defn solve [points]
  (->>
   (map line-segment points)
   (reduce
    (partial reduce (fn [diagram [x y]] (update-in diagram [y x] inc)))
    diagram)
   flatten
   (filter #(> % 1))
   count))

; part 1
(solve straight-line-points)

; part 2
(solve points)

(def points (->>
             (slurp "d5.txt")
             (clojure.string/split-lines)
             (map #(re-seq #"\d+" %))
             (map (fn [s] (map #(Integer/parseInt %) s)))
             (map vec)))

(def straight-line-points
  (filter (fn [[x1 y1 x2 y2]] (or (= x1 x2) (= y1 y2))) points))

(def max-dim
  (let [xs (mapcat (fn [[x1 _ x2 _]] [x1 x2]) points)
        ys (mapcat (fn [[_ y1 _ y2]] [y1 y2]) points)]
    (inc (apply max (concat xs ys)))))

(def diagram
  (vec (for [_ (range max-dim)]
         (vec (take max-dim (repeat 0))))))

(defn inclusive-range [from to]
  (range from (inc to)))

(defn smart-inclusive-range [from to]
  (cond (< from to) (inclusive-range from to)
        (< to from) (reverse (inclusive-range to from))
        :else (repeat from)))

(defn line-segment [x1 y1 x2 y2]
  (map vector (smart-inclusive-range x1 x2) (smart-inclusive-range y1 y2)))

(defn solve [points]
  (->>
   (reduce (fn [d [x1 y1 x2 y2]]
             (reduce (fn [d [x y]] (update-in d [y x] inc)) d (line-segment x1 y1 x2 y2)))
           diagram
           points)
   (mapcat (fn [row] (filter #(> % 1) row)))
   (count)))

; part 1
(solve straight-line-points)

; part 2
(solve points)

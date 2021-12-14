(def input
  (->> (slurp "d13.txt")
       (#(clojure.string/split % #"\n\n"))
       (map clojure.string/split-lines)))

(def points
  (->> (first input)
       (map #(clojure.string/replace % \, \space))
       (map #(str "[" % "]"))
       (map read-string)))

(defn fold [v at]
  (if (< v at) v
      (- at (- v at))))

(def folds
  (->> (second input)
       (map #(subs % (count "fold along ")))
       (map #(clojure.string/split % #"="))
       (map (fn [[axis at-str]] (let [at (Integer/parseInt at-str)]
                                     (if (= axis "x")
                                       (fn [[x y]] [(fold x at) y])
                                       (fn [[x y]] [x (fold y at)])))))))

; part 1
(->>
 (map (first folds) points)
 (set)
 (count))

(defn flip-vert [points]
  (map (fn [[x y]] [x (- 5 y)]) points))

(defn rot-minus-90 [points]
  (let [max-y (apply max (map second points))]
    (map (fn [[x y]] [(- max-y y) x]) points)))

(defn render [points]
  (->> (let [xs (map first points)
             ys (map second points)
             max-x (apply max xs)
             max-y (apply max ys)]
         (vec (for [_ (range (inc max-x))]
                (vec (for [_ (range (inc max-y))] ".")))))
       (#(reduce (fn [matrix point] (update-in matrix point (constantly "#"))) % points))
       (map (partial apply str))
       (clojure.string/join "\n")))

; part 2
(->>
 (reduce #(map %2 %1) points folds)
 (flip-vert)
 (rot-minus-90)
 (render)
 (println))

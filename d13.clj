(def input
  (->> (slurp "d13.txt")
       (#(clojure.string/split % #"\n\n"))
       (map clojure.string/split-lines)))

(def points
  (->> (first input)
       (map (comp read-string #(str "[" % "]")))))

(defn fold [v on]
  (if (< v on) v
      (- on (- v on))))

(def folds
  (->> (second input)
       (map #(subs % (count "fold along ")))
       (map #(clojure.string/split % #"="))
       (map (fn [[axis on]]
              (let [on (Integer/parseInt on)]
                (case axis
                  "x" (fn [[x y]] [(fold x on) y])
                  "y" (fn [[x y]] [x (fold y on)])))))))

; part 1
(->>
 (map (first folds) points)
 set
 count)

(defn render [points]
  (->> (let [[xs ys] (apply map vector points)
             max-x (apply max xs), max-y (apply max ys)]
         (vec (repeat (inc max-y) (vec (repeat (inc max-x) ".")))))
       (#(reduce (fn [matrix [x y]] (assoc-in matrix [y x] "#")) % points))
       (map clojure.string/join)
       (clojure.string/join "\n")))

; part 2
(->>
 (reduce #(map %2 %1) points folds)
 (render)
 (println))

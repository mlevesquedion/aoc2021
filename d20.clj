(require 'clojure.set)

(def parts (clojure.string/split (slurp "d20.txt") #"\n\n"))

(def enhancement-algorithm
  (first parts))

(def grid
  (->> (second parts)
       clojure.string/split-lines
       (mapv vec)))

(defn square-positions [[row col]]
  (for [r (range (dec row) (+ row 2))
        c (range (dec col) (+ col 2))]
    [r c]))
(def square-positions (memoize square-positions))

(defn enhance [pixels]
  (->> pixels
       (map {\# \1 \. \0})
       clojure.string/join
       (#(Integer/parseInt % 2))
       (nth enhancement-algorithm)))
(def enhance (memoize enhance))

(defn enhance-pos [pos grid default]
  (->> (square-positions pos)
       (map #(get-in grid % default))
       enhance))

(defn next-default [default]
  (let [center [1 1]
        grid (vec (repeat 3 (vec (repeat 3 default))))]
    (enhance-pos center grid nil)))
(def next-default (memoize next-default))

(defn step [[grid dim default]]
  (let [dim (+ 2 dim)]
    [(vec (for [i (range dim)]
            (vec (for [j (range dim)]
                   (enhance-pos [(dec i) (dec j)] grid default)))))
     dim
     (next-default default)]))

(defn solve [n]
  (->> (nth (iterate step [grid (count grid) \.]) n)
       first
       flatten
       (keep {\# 1})
       (apply +)))

; part 1
(solve 2)

; part 2
(solve 50)

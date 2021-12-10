(require 'clojure.set)

(def input (->> (slurp "d9.txt")
                (clojure.string/split-lines)
                (map char-array)
                (map (partial map #(- (int %) (int \0))))
                (map vec)
                (vec)))

(def rows (count input))
(def cols (count (first input)))
(def points (for [i (range rows) j (range cols)] [i j]))

(defn height [[i j]]
  (get-in input [i j]))

(defn neighbors [[i j]]
  (->> [[(dec i) j]
        [i (dec j)]
        [(inc i) j]
        [i (inc j)]]
       (filter (fn [[i j]] (and
                            (<= 0 i (dec rows))
                            (<= 0 j (dec cols)))))))

(defn low? [point]
  (let [h (height point)]
    (->> (neighbors point)
         (map height)
         (filter #(>= h %))
         (count)
         (= 0))))

(def low-points (filter low? points))

; part 1
(->> low-points
     (map height)
     (map inc)
     (apply +))

(defn dfs [[i j]]
  (loop [seen #{[i j]}
         [top & more] (list [i j])]
    (if (nil? top) seen
        (let [relevant-neighbors (filter #(and
                                           (not (seen %))
                                           (not= 9 (height %)))
                                         (neighbors top))]
          (recur (clojure.set/union seen (set relevant-neighbors))
                 (apply conj more relevant-neighbors))))))

; part 2
(->> low-points
     (map dfs)
     (map count)
     (sort >)
     (take 3)
     (apply *))

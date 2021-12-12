(defn map-matrix [f matrix]
  (vec (map vec (map (partial map f) matrix))))

(def input
  (->> (slurp "d11.txt")
       (clojure.string/split-lines)
       (map char-array)
       (map-matrix #(- (int %) (int \0)))))

(def rows 10)
(def cols 10)
(def points (for [i (range rows) j (range cols)] [i j]))

(defn adjacents [[i j :as pos]]
  (->> (for [ai (range (dec i) (+ i 2))
             aj (range (dec j) (+ j 2))] [ai aj])
       (set)
       (#(disj % pos))
       (filter (fn [[i j]] (and (<= 0 i (dec rows)) (<= 0 j (dec cols)))))))

(defn step [[acc matrix]]
  (let
   [matrix (map-matrix inc matrix)]
    (loop [[top & more] (filter #(> (get-in matrix %) 9) points)
           flashed #{}
           matrix matrix]
      (cond (nil? top) [(+ acc (count flashed)) matrix]
            (flashed top) (recur more flashed matrix)
            :else (let [not-flashed (filter (complement flashed) (adjacents top))
                        new-flashed (filter #(>= (get-in matrix %) 9) not-flashed)]
                    (->>
                     (update-in matrix top (constantly 0))
                     (#(reduce (fn [matrix point] (update-in matrix point inc)) % not-flashed))
                     (recur (concat more new-flashed) (conj flashed top))))))))

; part 1
(first (nth (iterate step [0 input]) 100))

(defn synchronized [matrix]
  (every? #(every? (partial = 0) %) matrix))

(defn steps-to-sync [matrix]
  (count (take-while (complement synchronized) (map second (iterate step [0 input])))))

; part 2
(steps-to-sync input)

(require 'clojure.string)

(defn map-matrix [f matrix]
  (mapv (partial mapv f) matrix))

(def input
  (->> (slurp "d11.txt")
       clojure.string/split-lines
       (map-matrix #(- (int %) (int \0)))))

(def rows (count input))
(def cols (count (first input)))
(def points (for [i (range rows) j (range cols)] [i j]))

(defn adjacents [[i j :as pos]]
  (->> (for [i (range (dec i) (+ i 2))
             j (range (dec j) (+ j 2))] [i j])
       set
       (#(disj % pos))
       (filter (fn [[i j]] (and (<= 0 i (dec rows)) (<= 0 j (dec cols)))))))

(defn step [[total-flashed matrix]]
  (let [matrix (map-matrix inc matrix)]
    (loop [[flashing & more-flashing] (filter #(> (get-in matrix %) 9) points)
           flashed #{}
           matrix matrix]
      (cond (nil? flashing) [(+ total-flashed (count flashed)) matrix]
            (flashed flashing) (recur more-flashing flashed matrix)
            :else (let [not-flashed (remove flashed (adjacents flashing))
                        new-flashed (filter #(>= (get-in matrix %) 9) not-flashed)]
                    (->>
                     (assoc-in matrix flashing 0)
                     (#(reduce (fn [matrix point] (update-in matrix point inc)) % not-flashed))
                     (recur (concat more-flashing new-flashed) (conj flashed flashing))))))))

; part 1
(first (nth (iterate step [0 input]) 100))

(defn synchronized [matrix]
  (= (apply + (apply concat matrix)) 0))

(defn steps-to-sync [matrix]
  (count (take-while (complement synchronized) (map second (iterate step [0 matrix])))))

; part 2
(steps-to-sync input)

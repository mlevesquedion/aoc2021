(require '[clojure.data.priority-map :refer [priority-map]])

(def input
  (->> (slurp "d15.txt")
       (clojure.string/split-lines)
       (map (comp vec (partial map #(- (int %) (int \0))) char-array))
       (vec)))

(defn update-neighbors [heap matrix [i j] ij-dist]
  (reduce
   (fn [h neighbor]
     (if-let [risk (get-in matrix neighbor)]
       (let [new-dist (+ ij-dist risk)
             cur-dist (or (get heap neighbor) ##Inf)]
         (if (>= new-dist cur-dist) h
             (assoc h neighbor new-dist)))
       h))
   heap
   [[(dec i) j] [i (dec j)] [(inc i) j] [i (inc j)]]))

(defn solve [matrix]
  (let [rows (dec (count matrix))
        cols (dec (count (first matrix)))]
    (loop [heap (priority-map [0 0] 0)
           seen? #{}]
      (let [[min-pos min-dist] (peek heap)]
        (cond (contains? heap [rows cols]) (get heap [rows cols])
              (seen? min-pos) (recur (pop heap) seen?)
              :else (recur (update-neighbors heap matrix min-pos min-dist) (conj seen? min-pos)))))))

; part 1
(solve input)

(defn digit-inc [n]
  (if (= n 9) 1 (inc n)))

(defn expand-row [row]
  (->>
   (reduce (fn [acc _] (conj acc (vec (map digit-inc (first acc)))))
           (list row)
           (range 4))
   (reverse)
   (apply concat)
   (vec)))

(defn transpose [matrix]
  (vec (apply map vector matrix)))

(defn expand [matrix]
  (->> matrix
       (map expand-row)
       (transpose)
       (map expand-row)
       (transpose)))

; part 2
(solve (expand input))

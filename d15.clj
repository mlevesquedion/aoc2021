(require '[clojure.data.priority-map :refer [priority-map]])

(def input
  (->> (slurp "d15.txt")
       clojure.string/split-lines
       (mapv (partial mapv #(- (int %) (int \0))))))

(defn update-neighbors [pqueue seen? matrix [i j] ij-dist]
  (reduce
   (fn [h neighbor]
     (if-let [risk (and (not (seen? neighbor)) (get-in matrix neighbor))]
       (let [new-dist (+ ij-dist risk)
             cur-dist (or (pqueue neighbor) ##Inf)]
         (if (< new-dist cur-dist) (assoc h neighbor new-dist)
             h))
       h))
   pqueue
   [[(dec i) j] [i (dec j)] [(inc i) j] [i (inc j)]]))

(defn solve [matrix]
  (let [last-row (dec (count matrix))
        last-col (dec (count (first matrix)))
        want-pos [last-row last-col]]
    (loop [pqueue (priority-map [0 0] 0)
           seen? #{}]
      (let [[min-pos min-dist] (peek pqueue)
            pqueue (pop pqueue)]
        (if (= min-pos want-pos) min-dist
            (recur (update-neighbors pqueue seen? matrix min-pos min-dist) (conj seen? min-pos)))))))

; part 1
(solve input)

(defn digit-inc [n]
  (inc (mod n 9)))

(defn expand-row [row]
  (->> row
       (iterate (partial map digit-inc))
       (take 5)
       (apply concat)))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn expand [matrix]
  (->> matrix
       (mapv expand-row)
       (transpose)
       (mapv expand-row)
       (transpose)))

; part 2
(solve (expand input))

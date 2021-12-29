(def grid
  (->> (slurp "d25.txt")
       clojure.string/split-lines
       (mapv vec)))

(defn move-line [cucumber line]
  (reduce-kv
   (fn [acc i v]
     (let [next-i (mod (inc i) (count line))]
       (if (and (= (nth line i) cucumber) (= (nth line next-i) \.))
         (-> acc
             (assoc i \.)
             (assoc next-i cucumber))
         acc)))
   line
   line))

(defn move-all [cucumber grid]
  (mapv (partial move-line cucumber) grid))

(defn transpose [grid]
  (apply mapv vector grid))

(defn step [grid]
  (->> grid
       (move-all \>)
       transpose
       (move-all \v)
       transpose))

; part 1
(let [iters (iterate step grid)]
  (->> (map vector iters (rest iters))
       (take-while (partial apply not=))
       count
       inc))

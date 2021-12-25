(def grid
  (->> (slurp "d25.txt")
       clojure.string/split-lines
       (mapv (partial apply vector))))

(defn move-line [line cucumber]
  (mapv (fn [i]
          (let [l (nth line (mod (dec i) (count line)))
                m (nth line i)
                r (nth line (mod (inc i) (count line)))]
            (cond (and (= l cucumber) (= m \.)) cucumber
                  (and (= m cucumber) (= r \.)) \.
                  :else m)))
        (range (count line))))

(defn move-all [grid cucumber]
  (vec (for [line grid]
         (move-line line cucumber))))

(defn transpose [grid]
  (vec (apply map vector grid)))

(defn move-east [grid]
  (move-all grid \>))

(defn move-south [grid]
  (-> grid
      transpose
      (move-all \v)
      transpose))

(defn step [grid]
  (->> grid
       move-east
       move-south))

; part 1
(time (let [iters (iterate step grid)]
        (->> (map vector iters (rest iters))
             (take-while (partial apply not=))
             count
             inc)))

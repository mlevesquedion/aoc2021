(def numbers (with-open [r (clojure.java.io/reader "d4.txt")]
               (->> (line-seq r)
                    (take 1)
                    (first)
                    (#(clojure.string/split % #","))
                    (map #(Integer/parseInt %)))))

(defn transpose [board]
  (apply map vector board))

(defn num-positions [board]
  (apply merge (for [i (range (count board)) j (range (count (first board)))]
                 {(get-in board [i j]) {:row i :col j}})))

(num-positions [[1 2] [3 4]])

(defn make-bingo-tracker [board]
  (let [rows (vec (map set board))
        cols (vec (map set (transpose board)))]
    {:rows rows
     :cols cols
     :nums (num-positions board)}))

(def bingo-trackers
  (with-open [r (clojure.java.io/reader "d4.txt")]
    (doall
     (loop [lines (drop 2 (line-seq r))
            bs []]
       (if (empty? lines) bs
           (let [b (->>
                    (take 5 lines)
                    (map #(re-seq #"\d+" %))
                    (map (fn [ss] (vec (map #(Integer/parseInt %) ss))))
                    (vec)
                    (make-bingo-tracker))]
             (recur (drop 6 lines) (conj bs b))))))))

(defn bingo? [bt]
  (or (some empty? (:rows bt)) (some empty? (:cols bt))))

(defn call-number [n bt]
  (let [{row :row col :col} (get-in bt [:nums n])]
    (if (nil? row) bt
        (-> bt
            (update-in [:rows row] #(disj % n))
            (update-in [:cols col] #(disj % n))))))

(defn unmarked-sum [bt]
  (apply + (clojure.set/union
            (apply clojure.set/union (:rows bt))
            (apply clojure.set/union (:cols bt)))))

(defn score [bt n]
  (* (unmarked-sum bt) n))

; part 1
(loop [nums numbers
       bts bingo-trackers]
  (let [this-num (first nums)
        next-bts (map (partial call-number this-num) bts)
        winner (some #(and (bingo? %) %) next-bts)]
    (if (not (nil? winner)) (score winner this-num)
        (recur (rest nums) next-bts))))

; part 2
(loop [nums numbers
       bts bingo-trackers]
  (let [this-num (first nums)
        next-bts (map (partial call-number this-num) bts)
        {not-bingos true [fst & rst] false} (group-by (comp nil? bingo?) next-bts)]
    (if (and (= 1 (count next-bts)) (not (nil? fst))) (score fst this-num)
        (recur (rest nums) not-bingos))))

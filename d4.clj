(def numbers (with-open [r (clojure.java.io/reader "d4.txt")]
               (->> (line-seq r)
                    first
                    (#(str \( % \)))
                    read-string)))

(defn transpose [board]
  (apply map vector board))

(defn num-positions [board]
  (apply merge (for [i (range (count board))
                     j (range (count (first board)))
                     :let [pos [i j]]]
                 {(get-in board pos) pos})))

(defn board->bingo-tracker [board]
  {:rows (mapv set board)
   :cols (mapv set (transpose board))
   :num->pos (num-positions board)})

(defn lines->bingo-tracker [lines]
  (->> lines
       (map #(re-seq #"\d+" %))
       (mapv (fn [int-strs] (mapv #(Integer/parseInt %) int-strs)))
       board->bingo-tracker))

(def bingo-trackers
  (->> (slurp "d4.txt")
       clojure.string/split-lines
       (drop 2)
       (remove empty?)
       (partition 5)
       (map lines->bingo-tracker)))

(defn bingo? [{:keys [rows cols]}]
  (some empty? (concat rows cols)))

(defn call-number [n bt]
  (let [[row col] (get-in bt [:num->pos n])
        remove-n #(disj % n)]
    (if (nil? row) bt
        (-> bt
            (update-in [:rows row] remove-n)
            (update-in [:cols col] remove-n)))))

(defn sum-unmarked [{:keys [rows cols]}]
  (apply + (apply clojure.set/union (concat rows cols))))

(defn score [bt n]
  (* (sum-unmarked bt) n))

; part 1
(reduce
 (fn [bts n]
   (let [next-bts (map (partial call-number n) bts)
         winner (some #(and (bingo? %) %) next-bts)]
     (if (some? winner) (reduced (score winner n))
         next-bts)))
 bingo-trackers
 numbers)

; part 2
(reduce
 (fn [bts n]
   (let [next-bts (map (partial call-number n) bts)
         {not-bingos false [bingo] true} (group-by (comp some? bingo?) next-bts)]
     (if (and (= 1 (count next-bts)) (some? bingo)) (reduced (score bingo n))
         not-bingos)))
 bingo-trackers
 numbers)

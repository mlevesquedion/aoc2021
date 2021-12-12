(defn update-graph [graph [from to]]
  (if (contains? graph from)
    (update graph from #(conj % to))
    (assoc graph from [to])))

(defn make-graph [pairs]
  (reduce update-graph {} (concat pairs (map reverse pairs))))

(def input
  (->> (slurp "d12.txt")
       (clojure.string/split-lines)
       (map #(clojure.string/split % #"-"))
       (make-graph)))

(defn small? [s]
  (apply <= (map int [\a (first s) \z])))
(def big? (complement small?))

(defn count-paths
  ([graph] (count-paths graph "start" #{"start"}))
  ([graph top seen]
   (if (= top "end") 1
       (let [neighbors (filter (complement seen) (graph top))]
         (if (empty? neighbors) 0
             (apply + (map #(count-paths graph %
                                         (if (small? %) (conj seen %) seen))
                           neighbors)))))))

; part 1
(count-paths input)

(defn count-paths-with-choice
  ([graph] (count-paths-with-choice graph "start" #{"start"} nil))
  ([graph top seen chosen]
   (if (= top "end")
     ; if a small cave was chosen but not seen, then there is an identical path where it was seen but not chosen -- avoid counting the same path twice
     (if (and chosen (not (seen chosen))) 0 1)
     (let [neighbors (filter (complement seen) (graph top))]
       (if (empty? neighbors) 0
           (apply + (map #(cond (big? %) (count-paths-with-choice graph % seen chosen)
                                chosen (count-paths-with-choice graph % (conj seen %) chosen)
                                :else (+ (count-paths-with-choice graph % (conj seen %) nil)
                                         (count-paths-with-choice graph % seen %)))
                         neighbors)))))))
; part 2
(count-paths-with-choice input)

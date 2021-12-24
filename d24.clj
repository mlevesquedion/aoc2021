(def code
  (->> (slurp "d24.txt")
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))))

(def table
  (->> (partition 18 code)
       (map #(map (vec %) [4 5 15]))
       (map (fn [triple] (map #(nth % 2) triple)))
       (map (partial map #(Integer/parseInt %)))))

(defn step [w z m a b]
  (let [x (+ (mod z 26) a)
        x (if (not= x w) 1 0)
        z (quot z m)
        z (* z (+ (* x 25) 1))
        z (+ z (* (+ w b) x))]
    z))

(defn next-ws [range- z m a]
  (if (= m 26)
    (let [w (+ (mod z 26) a)]
      (if (<= 1 w 9) [w]))
    range-))

(defn solve
  ([range-] (solve range- 0 [] table))
  ([range- z acc [[m a b] & table]]
   (if (nil? m)
     (if (= z 0) [(Long/parseLong (apply str acc))])
     (apply concat (for [w (next-ws range- z m a)]
                     (solve range- (step w z m a b) (conj acc w) table))))))

; part 1
(time (first (solve (range 9 0 -1))))

; part 2
(time (first (solve (range 1 10))))

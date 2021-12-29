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

(defn next-ws [z m a]
  (if (= m 26)
    (let [w (+ (mod z 26) a)]
      (if (<= 1 w 9) [w]))
    (range 1 10)))

(defn solve []
  (defn go [z acc [[m a b] & table]]
    (if (nil? m)
      (if (= z 0) [(Long/parseLong (apply str acc))])
      (mapcat
       #(go (step % z m a b) (conj acc %) table)
       (next-ws z m a))))
  (go 0 [] table))

; part 1
(last (solve))

; part 2
(first (solve))

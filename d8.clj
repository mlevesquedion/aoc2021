(require 'clojure.set
         'clojure.string)

(def input (->> (slurp "d8.txt")
                clojure.string/split-lines
                (map #(clojure.string/split % #" \| "))
                (map (partial map #(clojure.string/split % #" ")))
                (map (partial map (partial map set)))))

(def signals (map first input))
(def outputs (map second input))

; part 1
(count (filter #{2 3 4 7} (map count (flatten outputs))))

(defn having-count [n]
  (fn [coll] ({n coll} (count coll))))

(defn has-one-more? [more less]
  (and (empty? (clojure.set/difference less more))
       ((having-count 1) (clojure.set/difference more less))))

(defn having-one-more [less]
  (fn [more]
    (and (has-one-more? more less) more)))

(defn having-one-less [more]
  (fn [less]
    (and (has-one-more? more less) less)))

(defn make-decoder [signals]
  (let [one (some (having-count 2) signals)
        seven (some (having-count 3) signals)
        four (some (having-count 4) signals)
        eight (some (having-count 7) signals)
        nine-minus-bottom (clojure.set/union four (clojure.set/difference seven one))
        nine (some (having-one-more nine-minus-bottom) signals)
        three-minus-middle (clojure.set/difference nine (clojure.set/difference four one))
        three (some (having-one-more three-minus-middle) signals)
        six-minus-botleft (clojure.set/difference eight one)
        six (some (having-one-more six-minus-botleft) signals)
        five (some (having-one-less six) signals)
        middle (clojure.set/difference three three-minus-middle)
        zero (clojure.set/difference eight middle)
        two (first (clojure.set/difference (set signals) #{zero one three four five six seven eight nine}))]
    (zipmap [zero one two three four five six seven eight nine] (range 10))))

; part 2
(->> signals
     (map make-decoder)
     (map #(map %2 %1) outputs)
     (map (comp #(Integer/parseInt %) clojure.string/join))
     (apply +))

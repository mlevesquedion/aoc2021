(def lines (clojure.string/split-lines (slurp "d14.txt")))

(def template (first lines))

(def initial-pair-counts
  (->> (partition 2 1 template)
       frequencies))

(def insertion-rules
  (->> (drop 2 lines)
       (map #(clojure.string/split % #" -> "))
       (map (fn [[[x y] [z]]] {[x y] z}))
       (apply merge)))

(defn step [pair-counts]
  (->> (map (fn [[[x y :as p] c]]
              (let [z (insertion-rules p)]
                {[x z] c [z y] c})) pair-counts)
       (apply merge-with +)))

(defn ->item-counts [pair-counts]
  (->> (map (fn [[[x _] c]] {x c}) pair-counts)
       (apply merge-with +)
       (#(update % (last template) (fnil inc 0)))))

(defn solve [n]
  (let [pair-counts (nth (iterate step initial-pair-counts) n)
        counts (map val (->item-counts pair-counts))]
    (- (apply max counts) (apply min counts))))

; part 1
(solve 10)

; part 2
(solve 40)

(require 'clojure.string)

(defn make-graph [pairs]
  (reduce (fn [graph [from to]] (update graph from (fnil conj []) to)) {} (concat pairs (map reverse pairs))))

(def input
  (->> (slurp "d12.txt")
       clojure.string/split-lines
       (map #(clojure.string/split % #"-"))
       make-graph))

(defn small? [s]
  (= (clojure.string/lower-case s) s))

(def big? (complement small?))

(defn count-paths [graph]
  (defn go [curr seen]
    (if (= curr "end") 1
        (let [neighbors (remove seen (graph curr))]
          (if (empty? neighbors) 0
              (apply + (map #(go % (if (small? %) (conj seen %) seen)) neighbors))))))
  (go "start" #{"start"}))

; part 1
(count-paths input)

(defn count-paths-with-choice [graph]
  (defn go [curr seen chosen]
    ; if a small cave was chosen but not seen, then there is an identical path where it was seen but not chosen -- avoid counting the same path twice
    (if (= curr "end") (if (and chosen (not (seen chosen))) 0 1)
        (let [neighbors (remove seen (graph curr))]
          (if (empty? neighbors) 0
              (apply + (map #(cond (big? %) (go % seen chosen)
                                   chosen (go % (conj seen %) chosen)
                                   :else (+ (go % (conj seen %) nil)
                                            (go % seen %)))
                            neighbors))))))
  (go "start" #{"start"} nil))

; part 2
(count-paths-with-choice input)

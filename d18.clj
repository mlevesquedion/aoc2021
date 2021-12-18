(defn tokenize [s]
  (loop [remaining s
         tokens []]
    (let [[fst & rst] remaining]
      (if (empty? remaining) tokens
          (recur rst (conj tokens
                           (if (Character/isDigit fst) (- (int fst) (int \0))
                               fst)))))))

(def input
  (->> (slurp "d18.txt")
       clojure.string/split-lines
       (map tokenize)))

(defn add-first [n tokens]
  (loop [tokens tokens
         left []]
    (if (empty? tokens) left
        (let [[fst & rst] tokens]
          (if (int? fst) (concat left [(+ fst n)] rst)
              (recur rst (conj left fst)))))))

(defn explode [left-acc left remaining]
  (let [[_ right _ & rst] remaining]
    [(->> (add-first left (rest (reverse left-acc)))
          (cons 0)
          (reverse)
          (vec))
     (add-first right rst)]))

(defn explode-all [s]
  (loop [remaining s
         left-acc []
         nesting-level 0]
    (if (nil? remaining) left-acc
        (let [[fst & rst] remaining]
          (if (= nesting-level 5)
            (let [[left right] (explode left-acc fst rst)] (recur right left (dec nesting-level)))
            (recur rst (conj left-acc fst) (+ nesting-level (case fst \[ 1 \] -1 0))))))))

(defn split [left-acc left remaining]
  (let [n1 (quot left 2)
        n2 (- left n1)]
    (concat left-acc [\[ n1 \, n2 \]] remaining)))

(defn reduce-step
  ([[_ s]]
   (loop [remaining (explode-all s)
          left-acc []]
     (if (nil? remaining) [false left-acc]
         (let [[fst & rst] remaining]
           (if (and (int? fst) (> fst 9)) [true (split left-acc fst rst)]
               (recur rst (conj left-acc fst))))))))

(defn reduce-number [s]
  (->> (iterate reduce-step [true s])
       (drop-while first)
       first
       second))

(defn add-numbers [n1 n2]
  (reduce-number (vec (concat [\[] n1 [\,] n2 [\]]))))

(defn number->vec [n]
  (read-string (apply str n)))

(defn vec-magnitude [v]
  (if (int? v) v
      (+ (* 3 (vec-magnitude (first v))) (* 2 (vec-magnitude (second v))))))

(def magnitude (comp vec-magnitude number->vec))

; part 1
(magnitude (reduce add-numbers input))

(defn pairs [s]
  (if (empty? s) s
      (concat (map vector (repeat (first s)) (rest s)) (pairs (rest s)))))

; part 2
(->> (pairs input)
     (map (comp magnitude (partial apply add-numbers)))
     (apply max))

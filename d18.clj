(defn tokenize [line]
  (map #(if (Character/isDigit %) (- (int %) (int \0)) %) line))

(def input
  (->> (slurp "d18.txt")
       clojure.string/split-lines
       (map tokenize)))

(defn add-to-last-int [n tokens]
  (loop [i (dec (count tokens))]
    (cond (= -1 i) tokens
          (int? (get tokens i)) (update tokens i (partial + n))
          :else (recur (dec i)))))

(defn add-to-first-int [n tokens]
  (defn go [[tok & tokens]]
    (cond
      (nil? tok) ()
      (int? tok) (cons (+ n tok) tokens)
      :else (cons tok (go tokens))))
  (go tokens))

(defn explode [left n1 right]
  (let [[_ n2 _ & right] right]
    [(conj (pop (add-to-last-int n1 left)) 0)
     (add-to-first-int n2 right)]))

(defn explode-all [tokens]
  (loop [left []
         right tokens
         nesting-level 0]
    (if (nil? right) left
        (let [[tok & right] right]
          (if (= nesting-level 5)
            (let [[left right] (explode left tok right)] (recur left right (dec nesting-level)))
            (recur (conj left tok) right (+ nesting-level (case tok \[ 1 \] -1 0))))))))

(defn split [left n right]
  (let [n1 (quot n 2)
        n2 (- n n1)]
    (concat left [\[ n1 \, n2 \]] right)))

(defn reduce-step [[_ tokens]]
  (loop [left []
         tokens (explode-all tokens)]
    (if (nil? tokens) [false left]
        (let [[tok & tokens] tokens]
          (if (and (int? tok) (> tok 9)) [true (split left tok tokens)]
              (recur (conj left tok) tokens))))))

(defn reduce-number [s]
  (->> (iterate reduce-step [true s])
       (drop-while first)
       first
       second))

(defn add-numbers [n1 n2]
  (reduce-number (concat [\[] n1 [\,] n2 [\]])))

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

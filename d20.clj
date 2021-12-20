(require 'clojure.set)

(def parts (clojure.string/split (slurp "d20.txt") #"\n\n"))

(def enhancement-algorithm
  (vec (clojure.string/replace (first parts) "\n" "")))

(def pos->val
  (loop [row 0
         col 0
         remaining (second parts)
         asc {}]
    (let [[fst & more] remaining]
      (cond (empty? remaining) asc
            (= fst \newline) (recur (inc row) 0 more asc)
            :else (recur row (inc col) more (assoc asc [row col] fst))))))

(defn expand-pos [[row col]]
  (for [r (range (dec row) (+ row 2))
        c (range (dec col) (+ col 2))]
    [r c]))

(defn expand [positions]
  (reduce #(clojure.set/union %1 (set (expand-pos %2))) #{} positions))

(defn enhance-pos [pos pos->val default]
  (->> (expand-pos pos)
       (map #(get pos->val % default))
       (map {\# \1 \. \0})
       clojure.string/join
       (#(Integer/parseInt % 2))
       (nth enhancement-algorithm)))

(defn next-default [default]
  (let [center [1 1]
        pos->val (zipmap (expand-pos center) (repeat default))]
    (enhance-pos center pos->val nil)))

(defn enhance [[default pos->val]]
  (let [positions (vec (expand (expand (map key pos->val))))]
    [(next-default default) (reduce (fn [m pos] (assoc m pos (enhance-pos pos pos->val default))) {} positions)]))

(defn solve [n]
  (->> (nth (iterate enhance [\. pos->val]) n)
       (second)
       (map val)
       (filter (partial = \#))
       count))

; part 1
(solve 2)

; part 2
(solve 50)

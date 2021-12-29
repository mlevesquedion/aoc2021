(require 'clojure.set)

(defn parse-point [line]
  (read-string (str \[ line \])))

(def scanner-points
  (->> (slurp "d19.txt")
       (#(clojure.string/split % #"\n\n"))
       (map (comp (partial map parse-point) rest clojure.string/split-lines))
       (mapv set)))

(def identity-matrix [[1 0 0] [0 1 0] [0 0 1]])

(defn vec-sub [v1 v2]
  (mapv - v1 v2))

(defn dot [v1 v2]
  (apply + (map * v1 v2)))

(defn apply-matrix-to-point [mat point]
  (mapv (partial dot point) mat))

(defn permutations [s]
  (cond
    (not (set? s)) (permutations (set s))
    (empty? s) [[]]
    :else (mapcat
           (fn [x]
             (map (fn [perm] (conj perm x)) (permutations (disj s x))))
           s)))

(defn apply-orientation [orientation points]
  (mapv (partial mapv * orientation) points))

(defn det [[[a b c] [d e f] [g h i]]]
  (+ (* a (- (* e i) (* f h)))
     (- (* b (- (* d i) (* f g))))
     (* c (- (* d h) (* e g)))))

(def orientation-matrices
  (filter #(> (det %) 0)
          (for [x [-1 1] y [-1 1] z [-1 1]
                p (permutations identity-matrix)]
            (apply-orientation [x y z] p))))

(defn matching-dist [ps1 ps2]
  (->> (apply merge-with concat (for [p1 ps1 p2 ps2] {(vec-sub p1 p2) [p2]}))
       (some #(and (>= (count (val %)) 12) (key %)))))

(defn all-orientations [ps]
  (for [om orientation-matrices] (map #(apply-matrix-to-point om %) ps)))

(defn find-match [ps1 ps2]
  (some identity (for [ps2 (all-orientations ps2)]
                   (let [d (matching-dist ps1 ps2)]
                     (if d [d (set (map #(mapv + % d) ps2))])))))

(defn l1-norm [v]
  (apply + (map #(Math/abs %) v)))

(defn pairs [s]
  (if (empty? s) s
      (concat (map vector (repeat (first s)) (rest s))
              (pairs (rest s)))))

(let [[points scanner-positions]
      (loop [all-points (nth scanner-points 0)
             remaining-scanners (into clojure.lang.PersistentQueue/EMPTY scanner-points)
             scanner-positions []]
        (if (empty? remaining-scanners) [all-points scanner-positions]
            (let [scanner (peek remaining-scanners)
                  remaining-scanners (pop remaining-scanners)
                  [distance matched] (find-match all-points scanner)]
              (if (nil? distance)
                (recur all-points (conj remaining-scanners scanner) scanner-positions)
                (recur (clojure.set/union all-points matched) remaining-scanners (conj scanner-positions distance))))))]
        ; part 1
  (println (count points))
        ; part 2
  (println (apply max (for [[p1 p2] (pairs scanner-positions)] (l1-norm (vec-sub p1 p2))))))

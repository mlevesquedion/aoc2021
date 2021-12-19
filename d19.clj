(require 'clojure.set)

(defn parse-point [line]
  (read-string (str \[ line \])))

(def scanners
  (->> (slurp "d19.txt")
       (#(clojure.string/split % #"\n\n"))
       (map clojure.string/split-lines)
       (map rest)
       (map (partial map parse-point))
       (map set)
       (vec)))

(def identity-matrix [[1 0 0] [0 1 0] [0 0 1]])

(defn vec-sub [[x1 y1 z1] [x2 y2 z2]]
  [(- x1 x2) (- y1 y2) (- z1 z2)])

(defn dot [v1 v2]
  (apply + (map * v1 v2)))

(defn mat-mul-point [mat point]
  (vec (map (partial dot point) mat)))

(defn permutations [s]
  (cond
    (empty? s) [[]]
    (not (set? s)) (permutations (set s))
    :else (mapcat (fn [x] (map #(conj % x) (permutations (disj s x)))) s)))

(defn apply-orientation [[x y z] points]
  (vec (map (fn [[px py pz]] [(* x px) (* y py) (* z pz)]) points)))

(defn det [[[a b c] [d e f] [g h i]]]
  (+ (* a (- (* e i) (* f h)))
     (- (* b (- (* d i) (* f g))))
     (* c (- (* d h) (* e g)))))

(def orientation-matrices
  (filter #(> (det %) 0) (for [x [-1 1] y [-1 1] z [-1 1]
                               p (permutations identity-matrix)]
                           (apply-orientation [x y z] p))))

(defn matching-dist [ps1 ps2]
  (->> (apply merge-with concat (for [p1 ps1 p2 ps2] {(vec-sub p1 p2) [p2]}))
       (some #(and (>= (count (val %)) 12) (key %)))))

(defn match [s1 s2]
  (some identity (for [om orientation-matrices]
                   (let [s2 (map #(mat-mul-point om %) s2)
                         d (matching-dist s1 s2)]
                     (and d [d (set (map #(vec (map + % d)) s2))])))))

(defn manhattan [v]
  (apply + (map #(Math/abs %) v)))

(defn pairs [s]
  (if (empty? s) s
      (concat (map vector (repeat (first s)) (rest s)) (pairs (rest s)))))

(let [[scanners positions] (loop [acc (first scanners)
                                        remaining (subvec scanners 1)
                                        positions []]
                                   (if (empty? remaining) [acc positions]
                                       (let [top (first remaining)
                                             more (subvec remaining 1)
                                             [d d-top] (match acc top)]
                                         (if (nil? d)
                                           (recur acc (conj more top) positions)
                                           (recur (clojure.set/union acc d-top) more (conj positions d))))))]
  ; part 1
        (println (count scanners))
  ; part 2
        (println (apply max (for [[p1 p2] (pairs positions)] (manhattan (vec-sub p1 p2))))))

(require 'clojure.set
         'clojure.string)

(def input (->> (slurp "d9.txt")
                clojure.string/split-lines
                (mapv (partial mapv #(- (int %) (int \0))))))

(def rows (count input))
(def cols (count (first input)))
(def points (for [i (range rows) j (range cols)] [i j]))

(defn height [point]
  (get-in input point))

(defn neighbors [[i j]]
  (cond-> []
    (> i 0) (conj [(dec i) j])
    (< i (dec rows)) (conj [(inc i) j])
    (> j 0) (conj [i (dec j)])
    (< j (dec cols)) (conj [i (inc j)])))

(defn low? [point]
  (let [h (height point)]
    (->> (neighbors point)
         (map height)
         (every? #(> % h)))))

(def low-points (filter low? points))

; part 1
(->> low-points
     (map (comp inc height))
     (apply +))

(defn basin [point]
  (loop [seen #{point}
         [top & more] [point]]
    (if (nil? top) seen
        (let [next-points (->> (neighbors top)
                               (remove seen)
                               (remove (comp (partial = 9) height)))]
          (recur (clojure.set/union seen (set next-points))
                 (concat more next-points))))))

; part 2
(->> low-points
     (map (comp count basin))
     (sort >)
     (take 3)
     (apply *))

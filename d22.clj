(def number-re #"(-?[0-9]+)")
(def range-pattern (str number-re #"\.\." number-re))
(def line-re (re-pattern (str "(on|off) x=" range-pattern ",y=" range-pattern ",z=" range-pattern)))

(def reboot-steps
  (->> (slurp "d22.txt")
       clojure.string/split-lines
       (mapcat #(re-seq line-re %))
       (map (fn [[_ onoff x1 x2 y1 y2 z1 z2]] {:action ({"on" 1 "off" -1} onoff) :ranges [[x1 x2] [y1 y2] [z1 z2]]}))
       (map (fn [x] (update x :ranges (partial mapv (partial mapv #(Integer/parseInt %))))))))

(def init-range [-50 50])

(defn clamp-range [[l1 r1 :as _limits] [l2 r2]]
  [(max l1 l2) (min r1 r2)])

(defn shift-range [[low _] [from to]]
  [(- from low) (- to low)])

(defn range-overlap [[l1 r1 :as left] [l2 r2 :as right]]
  (if (< l2 l1)
    (range-overlap right left)
    [l2 (min r1 r2)]))

(defn range-size [[from to]]
  (- (inc to) from))

(defn ranges-overlap? [left right]
  (> (range-size (range-overlap left right)) 0))

(def init-steps
  (->> reboot-steps
       (filter (comp (partial every? (partial ranges-overlap? init-range)) :ranges))
       (map (fn [x] (update x :ranges (partial map (comp (partial shift-range init-range) (partial clamp-range init-range))))))))

(defn cubes-overlap? [p1 p2]
  (every? (partial apply ranges-overlap?) (map vector p1 p2)))

(defn cube-overlap [[xs1 ys1 zs1] [xs2 ys2 zs2]]
  [(range-overlap xs1 xs2) (range-overlap ys1 ys2) (range-overlap zs1 zs2)])

(defn cube-contains? [C c]
  (= (cube-overlap C c) c))

(defn cube-volume [ranges]
  (apply * (map range-size ranges)))

(defn eval-cubes [cubes]
  (reduce (fn [acc {:keys [action ranges]}]
            (let [vol (cube-volume ranges)]
              (+ acc (* action vol))))
          0 cubes))

(defn solve [steps]
  (eval-cubes (reduce
               (fn [cubes {new-action :action new-ranges :ranges :as new-cube}]
                 (let [not-contained (remove (comp (partial cube-contains? new-ranges) :ranges) cubes)
                       overlaps (reduce
                                 (fn [acc {old-action :action old-ranges :ranges :as old-cube}]
                                   (if (cubes-overlap? old-ranges new-ranges)
                                     (conj acc {:action (* -1 old-action) :ranges (cube-overlap old-ranges new-ranges)})
                                     acc))
                                 [] not-contained)
                       with-overlaps (concat not-contained overlaps)]
                   (if (= new-action 1)
                     (conj with-overlaps new-cube)
                     with-overlaps)))
               []
               steps)))

; part 1
(solve init-steps)

; part 2
(solve reboot-steps)

(def number-re #"(-?[0-9]+)")
(def interval-pattern (str number-re #"\.\." number-re))
(def line-re (re-pattern (str "(on|off) x=" interval-pattern ",y=" interval-pattern ",z=" interval-pattern)))

(def reboot-steps
  (->> (slurp "d22.txt")
       clojure.string/split-lines
       (map #(re-matches line-re %))
       (map (fn [[_ onoff x1 x2 y1 y2 z1 z2]] {:action ({"on" 1 "off" -1} onoff) :intervals [[x1 x2] [y1 y2] [z1 z2]]}))
       (map (fn [m] (update m :intervals (partial mapv (partial mapv #(Integer/parseInt %))))))))

(defn clamp-interval [[l1 r1 :as _limits] [l2 r2]]
  [(max l1 l2) (min r1 r2)])

(defn shift-interval [by [from to]]
  [(- from by) (- to by)])

(defn interval-overlap [[l1 r1 :as left] [l2 r2 :as right]]
  (if (< l2 l1)
    (interval-overlap right left)
    [l2 (min r1 r2)]))

(defn interval-size [[from to]]
  (- (inc to) from))

(defn intervals-overlap? [left right]
  (> (interval-size (interval-overlap left right)) 0))

(def init-interval [-50 50])

(defn overlaps-init [{:keys [intervals]}]
  (every? (partial intervals-overlap? init-interval) intervals))

(defn restrict-to-init [m]
  (update m :intervals
          (partial map (fn [x] (->> x
                                    (clamp-interval init-interval)
                                    (shift-interval (first init-interval)))))))

(def init-steps
  (->> reboot-steps
       (filter overlaps-init)
       (map restrict-to-init)))

(defn prism-overlap [[xs1 ys1 zs1] [xs2 ys2 zs2]]
  [(interval-overlap xs1 xs2)
   (interval-overlap ys1 ys2)
   (interval-overlap zs1 zs2)])

(defn prisms-overlap? [p1 p2]
  (every? #(> (interval-size %) 0) (prism-overlap p1 p2)))

(defn prism-contains? [P p]
  (= (prism-overlap P p) p))

(defn prism-volume [intervals]
  (apply * (map interval-size intervals)))

(defn make-prisms [steps]
  (reduce
   (fn [prisms {new-action :action new-intervals :intervals :as new-prism}]
     (let [not-contained (remove (comp (partial prism-contains? new-intervals) :intervals) prisms)
           overlaps (reduce
                     (fn [acc {old-action :action old-intervals :intervals}]
                       (if (prisms-overlap? old-intervals new-intervals)
                         (conj acc {:action (* -1 old-action)
                                    :intervals (prism-overlap old-intervals new-intervals)})
                         acc))
                     [] not-contained)
           updated-prisms (concat not-contained overlaps)]
       (if (= new-action 1)
         (conj updated-prisms new-prism)
         updated-prisms)))
   []
   steps))

(defn eval-prisms [prisms]
  (reduce (fn [acc {:keys [action intervals]}]
            (let [vol (prism-volume intervals)]
              (+ acc (* action vol))))
          0 prisms))

(defn solve [steps]
  (->> steps
       make-prisms
       eval-prisms))

; part 1
(solve init-steps)

; part 2
(solve reboot-steps)

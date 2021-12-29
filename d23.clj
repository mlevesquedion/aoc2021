(require 'clojure.string
         '[clojure.data.priority-map :refer [priority-map]])

(def pod-cost {\A 1 \B 10 \C 100 \D 1000})
(def pod-column {\A 2 \B 4 \C 6 \D 8})

(def initial-hallway
  ; false represents an invalid position (above a room), nil represents an empty position
  [nil nil false nil false nil false nil false nil nil])

(defn create-state [lines]
  (let [[A B C D] (->> (drop 2 lines)
                       butlast
                       (map (partial filter (set "ABCD")))
                       (apply map vector)
                       (map (partial apply list)))]
    {\A A
     \B B
     \C C
     \D D
     :pods-per-room (count A)
     :hallway initial-hallway}))

(def lines (clojure.string/split-lines (slurp "d23.txt")))
(def p1-initial-state (create-state lines))

(def p2-extra-lines
  ["#D#C#B#A#"
   "#D#B#A#C#"])

(def p2-initial-state (create-state (concat (take 3 lines) p2-extra-lines (drop 3 lines))))

(defn abs-dist [x y]
  (Math/abs (- x y)))

(defn reachable-columns [hallway start-pos]
  (let [available? (comp not hallway)
        valid? (comp nil? hallway)
        before (range (dec start-pos) -1 -1)
        after (range (inc start-pos) 11)]
    (set (filter valid?
                 (concat (take-while available? before)
                         (take-while available? after))))))

(defn move-from-room-to-hallway [{:keys [hallway pods-per-room] :as state} room-pod]
  (let [[room-top & room-more :as room] (get state room-pod)]
    (if (apply = room-pod room) []
        (let [room-col (pod-column room-pod)
              cost-to-leave-room (- pods-per-room (count room-more))
              reachable-cols (reachable-columns hallway room-col)]
          (for [new-col reachable-cols]
            [(* (pod-cost room-top) (+ cost-to-leave-room (abs-dist room-col new-col)))
             (-> state
                 (assoc room-pod room-more)
                 (assoc-in [:hallway new-col] room-top))])))))

(defn can-enter-room? [room pod-positions [posn pod]]
  (let [room-col (pod-column pod)]
    (and
     (every? (complement (disj pod-positions posn)) (range (min posn room-col) (inc (max posn room-col))))
     (every? #{pod} room))))

(defn enumerate [s]
  (map vector (range) s))

(defn move-from-hallway-to-room [{:keys [hallway pods-per-room] :as state}]
  (let [hallway-pods (filter second (enumerate hallway))
        pod-positions (set (map first hallway-pods))]
    (keep (fn [[posn pod :as hallway-pod]]
            (let [room (get state pod)]
              (when (can-enter-room? room pod-positions hallway-pod)
                [(* (pod-cost pod)
                    (+ (abs-dist posn (pod-column pod))
                       (- pods-per-room (count room))))
                 (-> state
                     (update pod conj pod)
                     (assoc-in [:hallway posn] nil))])))
          hallway-pods)))

(defn moves [state]
  (concat
   (mapcat (partial move-from-room-to-hallway state) "ABCD")
   (move-from-hallway-to-room state)))

(defn is-solved? [{A \A B \B C \C D \D :keys [pods-per-room]}]
  (let [rooms [A B C D]]
    (and (every? #(= (count %) pods-per-room) rooms)
         (every? (fn [[room pod]] (apply = pod room)) (map vector rooms "ABCD")))))

(defn room-cost [room room-pod]
  (apply + (map (fn [pod] (if (= pod room-pod) 0
                              (* (pod-cost pod) (+ (abs-dist (pod-column pod) (pod-column room-pod)) 2))))
                room)))

(defn hallway-cost [hallway]
  (apply + (for [[posn pod] (filter second (enumerate hallway))]
             (* (pod-cost pod) (inc (abs-dist posn (pod-column pod)))))))

(defn heuristic [{A \A B \B C \C D \D :keys [hallway]}]
  (apply + (hallway-cost hallway) (map room-cost [A B C D] "ABCD")))

(defn solve [initial-state]
  (loop [[pqueue distance] [(priority-map initial-state 0) {initial-state 0}]
         seen? #{}]
    (let [[min-cost-state] (peek pqueue)
          pqueue (pop pqueue)]
      (if (is-solved? min-cost-state) (distance min-cost-state)
          (recur
           (reduce (fn [[pqueue distance :as acc] [move-cost move-state]]
                     (if (seen? move-state) acc
                         (let [current-dist (get distance move-state ##Inf)
                               move-dist (+ (distance min-cost-state) move-cost)]
                           (if (< move-dist current-dist)
                             [(assoc pqueue move-state (+ (heuristic move-state) move-dist))
                              (assoc distance move-state move-dist)]
                             acc))))
                   [pqueue distance]
                   (moves min-cost-state))
           (conj seen? min-cost-state))))))

; part 1
(solve p1-initial-state)

; part 2
(solve p2-initial-state)

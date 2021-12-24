(require '[clojure.data.priority-map :refer [priority-map]])

(def initial-hallway
  ; false represents an invalid position (above a room), nil represents an empty position
  [nil nil false nil false nil false nil false nil nil])

(defn create-state [lines]
  (let [[A B C D] (->> (drop 2 lines)
                       (butlast)
                       (map (partial filter #{\A \B \C \D}))
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
  ["  #D#C#B#A#"
   "  #D#B#A#C#"])

(def p2-initial-state (create-state (concat (take 3 lines) p2-extra-lines (drop 3 lines))))

(def pod-cost {\A 1 \B 10 \C 100 \D 1000})
(def room-column {\A 2 \B 4 \C 6 \D 8})

(defn reachable-columns [hallway start-pos]
  (filter (comp nil? hallway)
          (concat (take-while (comp (complement boolean) hallway) (range (dec start-pos) -1 -1))
                  (take-while (comp (complement boolean) hallway) (range (inc start-pos) 11)))))

(defn abs-dist [x y]
  (Math/abs (- x y)))

(defn move-from-room-to-hallway [{:keys [hallway pods-per-room] :as state} room-pod]
  (let [[top & more :as room] (get state room-pod)]
    (if (apply = room-pod room) []
        (let [room-col (room-column room-pod),
              cost-to-leave-room (- pods-per-room (count more)),
              reachable-cols (reachable-columns hallway room-col)]
          (for [col reachable-cols]
            [(* (pod-cost top) (+ cost-to-leave-room (abs-dist col room-col)))
             (-> state
                 (assoc room-pod more)
                 (assoc-in [:hallway col] top))])))))

(defn enumerate [s]
  (map vector (iterate inc 0) s))

(defn all-empty? [hallway a-col b-col]
  (every?
   #(not (boolean (nth hallway %)))
   (range (min a-col b-col) (inc (max a-col b-col)))))

(defn move-from-room-to-room [{:keys [hallway pods-per-room] :as state} room-pod]
  (let [[top & more :as room] (get state room-pod)]
    (if (apply = room-pod room) []
      (let [from-col (room-column room-pod)
            to-col (room-column top)
            cost-to-leave-room (- pods-per-room (count more))
            to-room (get state top)
            cost-to-enter-room (- pods-per-room (count to-room))]
        (if (and (all-empty? hallway from-col to-col) (< pods-per-room (count to-room)))
              [[(* (pod-cost top) (+ cost-to-leave-room cost-to-enter-room (abs-dist from-col to-col)))
               (-> state
                   (update top conj top)
                   (update room-pod rest))]]
              [])))))

(defn can-enter-room? [{:keys [pods-per-room] :as state} pod-positions [posn pod]]
  (let [room-col (room-column pod)]
    (and
     (every? (complement (disj pod-positions posn)) (range (min posn room-col) (inc (max posn room-col))))
     (every? #{pod} (get state pod))
     (not= (count (get state pod)) pods-per-room))))

(defn move-from-hallway-to-room [{:keys [hallway pods-per-room] :as state}]
  (let [hallway-pods (filter (comp boolean second) (enumerate hallway))
        pod-positions (set (map first hallway-pods))
        hallway-pods (filter (partial can-enter-room? state pod-positions) hallway-pods)]
    (map (fn [[posn pod]] [(* (pod-cost pod)
                              (+ (abs-dist posn (room-column pod))
                                 (- pods-per-room (count (get state pod)))))
                           (-> state
                               (update pod conj pod)
                               (assoc-in [:hallway posn] nil))])
         hallway-pods)))

(defn get-next-states [state]
  (concat
   (apply concat (map (partial move-from-room-to-hallway state) "ABCD"))
   (apply concat (map (partial move-from-room-to-room state) "ABCD"))
   (move-from-hallway-to-room state)))

(defn is-solved? [pods-per-room room pod]
  (and (= (set room) #{pod}) (= (count room) pods-per-room)))

(defn is-end-state? [{A \A B \B C \C D \D :keys [pods-per-room]}]
  (every? (partial apply is-solved? pods-per-room) (map vector [A B C D] "ABCD")))

(defn room-cost [room room-pod]
  (apply + (map (fn [pod] (if (= pod room-pod) 0
                              (* (pod-cost pod) (abs-dist (room-column pod) (room-column room-pod))))) room)))

(defn hallway-cost [hallway]
  (apply + (for [[posn pod] (filter (comp boolean second) (enumerate hallway))]
             (* (pod-cost pod) (abs-dist posn (room-column pod))))))

(defn heuristic [{A \A B \B C \C D \D :keys [hallway]}]
  (apply +
         (hallway-cost hallway)
         (map room-cost [A B C D] "ABCD")))

(defn solve [initial-state]
  (loop [[heap distance] [(priority-map initial-state 0) {initial-state 0}]
         visited? #{}]
    (let [[min-cost-state min-cost] (peek heap)]
      (if (is-end-state? min-cost-state) (distance min-cost-state)
          (recur
           (reduce (fn [[heap distance :as acc] [new-cost new-state]]
                     (if (visited? new-state) acc
                         (let [current-dist (get distance new-state ##Inf)
                               new-dist (+ (distance min-cost-state) new-cost)]
                           (if (< new-dist current-dist)
                             [(assoc heap new-state (+ (heuristic new-state) new-dist))
                              (assoc distance new-state new-dist)]
                             acc))))
                   [(pop heap) distance]
                   (get-next-states min-cost-state))
           (conj visited? min-cost-state))))))

; part 1
(solve p1-initial-state)

; part 2
(solve p2-initial-state)

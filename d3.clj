(def input
  (->>
   (slurp "d3.txt")
   clojure.string/split-lines))

(defn pick-by-count [picker s]
  (->> s
       (apply map vector) ; transpose
       (map (comp picker frequencies))))

(defn max-or-1 [{zeroes \0 ones \1}] (if (> zeroes ones) \0 \1))
(defn min-or-0 [{zeroes \0 ones \1}] (if (< ones zeroes) \1 \0))

(defn bits->int [bits]
  (Long/parseLong (apply str bits) 2))

(defn gamma-rate [bits]
  (->> bits
       (pick-by-count max-or-1)
       bits->int))

; part 1
(let [g (gamma-rate input)
      e (bit-and-not 0xFFF g)] ; https://stackoverflow.com/a/41984792
  (* g e))

(defn pick-candidate [picker candidates]
  (->> (loop [candidates candidates
              posn 0]
         (if (= (count candidates) 1) (first candidates)
             (let [pick (first (pick-by-count picker (map (fn [s] [(nth s posn)]) candidates)))]
               (recur (filter #(= pick (nth % posn)) candidates) (inc posn)))))
       bits->int))

; part 2
(let [o (pick-candidate max-or-1 input)
      co2 (pick-candidate min-or-0 input)]
  (* o co2))

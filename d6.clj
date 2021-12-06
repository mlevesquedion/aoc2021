(def lifetimes (->> (slurp "d6.txt")
                    (clojure.string/trim)
                    (#(clojure.string/split % #","))
                    (map #(Integer/parseInt %))))

(def new-bucket {:new 0 :old 0})

(defn bucketize [lifetimes]
  (let [init (vec (take 9 (repeat new-bucket)))]
    (reduce
     #(update-in %1 [%2 :old] inc)
     init lifetimes)))

(def buckets (bucketize lifetimes))

(defn bucket-sum [{:keys [new old]}]
  (+ new old))

(defn step [buckets]
  (let [new-fish (bucket-sum (first buckets))
        add-fish (partial + new-fish)
        new-buckets (conj (subvec buckets 1) new-bucket)]
    (-> new-buckets
        (update-in [6 :old] add-fish)
        (update-in [8 :new] add-fish))))

(defn solve [days]
  (->>
   (nth (iterate step buckets) days)
   (map bucket-sum)
   (apply +)))

; part 1
(solve 80)

; part 2
(solve 256)

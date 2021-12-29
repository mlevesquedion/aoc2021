(require 'clojure.string)

(def lifetimes (->> (slurp "d6.txt")
                    clojure.string/trim-newline
                    (#(clojure.string/split % #","))
                    (map #(Integer/parseInt %))))

(def buckets (frequencies lifetimes))

(def +nil (fnil + 0))

(defn step [buckets]
  (reduce
   (fn [next-buckets [age n]]
     (if (= age 0)
       (-> next-buckets
           (update 6 +nil n)
           (update 8 +nil n))
       (update next-buckets (dec age) +nil n)))
   {}
   buckets))

(defn solve [days]
  (->>
   (nth (iterate step buckets) days)
   vals
   (apply +)))

; part 1
(solve 80)

; part 2
(solve 256)

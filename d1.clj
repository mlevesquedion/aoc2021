(def input
  (->> (clojure.java.io/reader "d1.txt")
       (line-seq)
       (map read-string)))

(defn increases [s]
  (->> (map < s (rest s))
       (filter true?)
       (count)))

; part 1
(increases input)

; part 2
(->> input
     (partition 3 1)
     (map #(apply + %))
     (increases))

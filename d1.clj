(require 'clojure.string)

(def input
  (->> (slurp "d1.txt")
       clojure.string/split-lines
       (map read-string)))

(defn increases [s]
  (->> (map < s (rest s))
       (filter true?)
       count))

; part 1
(increases input)

; part 2
(->> input
     (partition 3 1)
     (map #(apply + %))
     increases)

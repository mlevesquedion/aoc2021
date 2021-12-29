(require 'clojure.string)

(def input
  (->> (slurp "d2.txt")
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))
       (map (fn [parts] (update parts 1 read-string)))))

(defn move [[horiz depth] [instr n]]
  (case instr
    "forward" [(+ horiz n) depth]
    "up" [horiz (- depth n)]
    "down" [horiz (+ depth n)]))

(->> input
     (reduce move [0 0])
     (apply *))

(defn move-and-aim [[horiz depth aim] [instr n]]
  (case instr
    "forward" [(+ horiz n) (+ depth (* aim n)) aim]
    "down" [horiz depth (+ aim n)]
    "up" [horiz depth (- aim n)]))

(->> input
     (reduce move-and-aim [0 0 0])
     (take 2)
     (apply *))

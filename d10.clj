(def input
  (->> (slurp "d10.txt")
       (clojure.string/split-lines)
       (map char-array)))

(def closing-bracket {\( \)
                      \[ \]
                      \{ \}
                      \< \>})

(def illegal-score {\) 3
                    \] 57
                    \} 1197
                    \> 25137})

(defn first-illegal-bracket [line]
  (loop [[top & more :as stack] '()
         [b & bs] line]
    (if (closing-bracket b) (recur (conj stack b) bs)
        (let [cb (closing-bracket top)]
          (if (= b cb) (recur more bs) b)))))

; part 1
(->> input
     (map first-illegal-bracket)
     (filter (complement nil?))
     (map illegal-score)
     (apply +))

(defn missing-brackets [line]
  (->> (reduce (fn [stack b]
                 (if (closing-bracket b) (conj stack b) (rest stack)))
               '() line)
       (map closing-bracket)))

(def missing-score {\) 1
                    \] 2
                    \} 3
                    \> 4})

(defn score-missing-brackets [bs]
  (reduce #(+ (* 5 %1) (missing-score %2)) 0 bs))

(defn median [xs]
  (nth (sort xs) (quot (count xs) 2)))

; part 2
(->> input
     (filter (comp nil? first-illegal-bracket))
     (map missing-brackets)
     (map score-missing-brackets)
     (median))

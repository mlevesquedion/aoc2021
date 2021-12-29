(def input
  (->> (slurp "d10.txt")
       clojure.string/split-lines))

(def closing-bracket
  {\( \), \[ \], \{ \}, \< \>})

(def illegal-score
  {\) 3, \] 57, \} 1197, \> 25137})

(defn first-illegal-bracket [line]
  (loop [[stack-top & stack-rest :as stack] '()
         [bracket & line-rest] line]
    (if (closing-bracket bracket) (recur (conj stack bracket) line-rest)
        (let [want-closing (closing-bracket stack-top)]
          (if (= bracket want-closing) (recur stack-rest line-rest) bracket)))))

; part 1
(->> input
     (keep first-illegal-bracket)
     (map illegal-score)
     (apply +))

(defn missing-brackets [line]
  (reduce (fn [stack b]
            (if-let [cb (closing-bracket b)] (conj stack cb) (rest stack)))
          '() line))

(def missing-score
  (zipmap ")]}>" (range 1 5)))

(defn score-missing-brackets [bs]
  (reduce #(+ (* 5 %1) (missing-score %2)) 0 bs))

(defn median [xs]
  (nth (sort xs) (quot (count xs) 2)))

; part 2
(->> input
     (filter (comp nil? first-illegal-bracket))
     (map missing-brackets)
     (map score-missing-brackets)
     median)

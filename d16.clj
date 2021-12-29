(def hex->bits
  {\0 "0000" \1 "0001" \2 "0010" \3 "0011"
   \4 "0100" \5 "0101" \6 "0110" \7 "0111"
   \8 "1000" \9 "1001" \A "1010" \B "1011"
   \C "1100" \D "1101" \E "1110" \F "1111"})

(def input
  (->>
   (slurp "d16.txt")
   (map hex->bits)
   clojure.string/join))

(defn bits->int [bits]
  (Long/parseLong (clojure.string/join bits) 2))

(declare parse-literal)
(declare parse-operator)
(defn parse-packet [bits]
  (let [[header bits] (split-at 6 bits)
        [version type-id] (map bits->int (partition 3 header))
        tags {:version version :type-id type-id}]
    (if (= type-id 4)
      (let [[value-bits bits] (parse-literal bits)]
        [(merge tags {:value (bits->int value-bits)}) bits])
      (let [[subpackets bits] (parse-operator bits)]
        [(merge tags {:sub-packets subpackets}) bits]))))

(defn drop-at-most-3-zeroes
  ([bits] (drop-at-most-3-zeroes bits 3))
  ([bits n] (if (or (= n 0) (= (first bits) \1)) bits
                (drop-at-most-3-zeroes bits (dec n)))))

(defn parse-literal [bits]
  (let [[[start & num-bits :as group] bits] (split-at 5 bits)]
    (case start
      \1 (let [[acc-bits remaining-bits] (parse-literal bits)
               value-bits (concat num-bits acc-bits)]
           [value-bits (drop-at-most-3-zeroes remaining-bits)])
      \0 [num-bits bits])))

(defn parse-packets [bits]
  (loop [packets []
         bits bits]
    (let [[packet bits] (parse-packet bits)
          packets (conj packets packet)]
      (if (empty? bits) packets
          (recur packets bits)))))

(defn parse-n-packets [n bits]
  (loop [packets []
         n n
         bits bits]
    (if (= n 0) [packets bits]
        (let [[packet bits] (parse-packet bits)]
          (recur (conj packets packet) (dec n) bits)))))

(defn parse-operator [bits]
  (let [[length-type-id & bits] bits]
    (case length-type-id
      \0 (let [[length-bits bits] (split-at 15 bits)
               [subpacket-bits bits] (split-at (bits->int length-bits) bits)]
           [(parse-packets subpacket-bits) bits])
      \1 (let [[num-bits bits] (split-at 11 bits)]
           (parse-n-packets (bits->int num-bits) bits)))))

(defn sum-versions [packet]
  (let [{:keys [version sub-packets]} packet]
    (apply + version (map sum-versions (or sub-packets [])))))

(def parsed (first (parse-packet input)))

; part 1
(sum-versions parsed)

(def bool->int #(if % 1 0))

(defn evaluate [packet]
  (let [{:keys [type-id value sub-packets]} packet]
    (apply
     (case type-id
       4 (constantly value)
       0 +
       1 *
       2 min
       3 max
       5 (comp bool->int >)
       6 (comp bool->int <)
       7 (comp bool->int =))
     (map evaluate (or sub-packets [])))))

; part 2
(evaluate parsed)

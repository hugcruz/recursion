(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq)
       (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (>(count seq-1) (count seq-2))
    seq-1
    seq-2
    ))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq)
       (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons
            (first a-seq)
            (my-filter pred? (rest a-seq))
            )
      (my-filter pred? (rest a-seq))
    )))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq))
    )))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons
            (first a-seq)
            (my-take-while pred? (rest a-seq))
            )
      '()
    )))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq
    )))

(defn seq= [a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    (= (count a-seq) (count b-seq))
    (if (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
      false
    )))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2))
    )))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else   (+ (fib (dec n)) (fib (- n 2)))
    ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)) )))

(defn tails [a-seq]
  (if (empty? a-seq)
    (vector a-seq)
    (cons (vec a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (vector a-seq)

    (cons (vec a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-n [a-seq n]
  (if (= n 0)
    nil
    (let [
           rotated (conj (vec (rest a-seq)) (first a-seq))
           ]
    (cons rotated
          (rotations-n rotated (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rotations-n a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
                             (if (contains? freqs (first a-seq))
                               (update-in freqs [(first a-seq)] inc)
                               (conj {(first a-seq) 1} freqs)
                             )
                             (rest a-seq)
                           )))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [
          entry (first a-map)
          k     (key entry)
          v     (val entry)
        ]
      (concat (repeat v k) (un-frequencies (rest a-map)))
    )))

(defn my-take [n coll]
  (if(or (= n 0) (empty? coll))
    '()
    (cons
      (first coll)
      (my-take (dec n) (rest coll))
    )))

(defn my-drop [n coll]
  (if(or (= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [
       half (int (/ (count a-seq) 2))
       ]
    (list (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)                  b-seq
    (empty? b-seq)                  a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else                           (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
  ))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [halves (halve a-seq)]
      (seq-merge (merge-sort (first halves)) (merge-sort (second halves)))
    )
  ))

(defn take-while-pair [f a-seq]
  (cond
    (= (count a-seq) 1) a-seq
    (f (first a-seq)(second a-seq))
                        (cons (first a-seq) (take-while-pair f (rest a-seq)))
    :else               (list (first a-seq))))


(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    nil
    (let [
        function  (if(< (first a-seq) (second a-seq))
                    <
                    >)
        monotonic (take-while-pair function a-seq)
        remaining (drop (count monotonic) a-seq)
         ]
      (cons monotonic (split-into-monotonics remaining)))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (apply concat (map (fn [x] (map
                                 #(cons (first x) %)
                                 (permutations (rest x))))
                       (rotations a-set)))))

(defn powerset [a-set]
  (if(empty? a-set)
    #{#{}}
    (let [currentset (powerset (rest a-set))]
      (clojure.set/union
        currentset
        (map #(conj % (first a-set))
             currentset)
))))

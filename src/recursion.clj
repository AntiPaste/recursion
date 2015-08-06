(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product(rest coll)))))

(defn singleton? [coll]
  (and
   (not (empty? coll))
   (empty? (rest coll))))

(defn my-last [coll]
  (if (or
       (singleton? coll)
       (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or
       (singleton? a-seq)
       (empty? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (or
       (singleton? a-seq)
       (empty? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence(rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (let [[x & xs] seq-1
          [y & ys] seq-2]
      (cons (f x y) (my-map f xs ys)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 2)) (fib (dec n)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (conj
     (my-repeat (dec how-many-times) what-to-repeat)
     what-to-repeat)))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons
     (dec up-to)
     (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (reverse (map reverse (tails (reverse a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [n (first a-seq)
          freqs-2 (if (contains? freqs n)
                    (assoc freqs n (inc (freqs n)))
                    (assoc freqs n 1))]
      (my-frequencies-helper freqs-2 (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[a-key a-value] (first a-map)]
      (concat (repeat a-value a-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or
       (empty? coll)
       (<= n 0))
    '()
    (conj (my-take (dec n) (rest coll)) (first coll))))

(defn my-drop [n coll]
  (if (or
       (empty? coll)
       (<= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [[x & xs] a-seq
        [y & ys] b-seq]
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (< x y) (cons x (seq-merge xs b-seq))
     :else (cons y (seq-merge a-seq ys)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])


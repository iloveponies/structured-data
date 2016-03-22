(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (max (first a-seq)
         (or (max-element (rest a-seq)) 0))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (> c1 c2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [x (first a-seq)
          r-seq (rest a-seq)]
      (if (pred? x)
        (cons x (my-filter pred? r-seq))
        (my-filter pred? r-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [f (first a-seq)]
      (if (pred? f)
        (cons f (my-take-while pred? (rest a-seq)))
        '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [f (first a-seq)]
      (if (pred? f)
        (my-drop-while pred? (rest a-seq))
        (seq a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (and (empty? a-seq) (not (empty? b-seq))) false
   (and (empty? b-seq) (not (empty? a-seq))) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if
   (or (empty? seq-1) (empty? seq-2)) '()
   (cons (f (first seq-1) (first seq-2))
         (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))


(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq)
          (inits (reverse (rest (reverse a-seq)))))))

(defn rot-helper [n a-seq]
  (cond
   (= n 0) '()
   :else (let [rot (concat (rest a-seq) [(first a-seq)])]
      (cons rot
            (rot-helper (dec n) rot)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rot-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          f (get freqs elem)
          new-freq (if (contains? freqs elem)
                     (assoc freqs elem (inc f))
                     (assoc freqs elem 1))]
      (my-frequencies-helper new-freq (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat
     (let [entry (first a-map)]
       (repeat (get entry 1) (get entry 0)))
     (un-frequencies (rest a-map)))))


(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (= 0 n)
    coll
    (my-drop (dec n) (rest coll))))


(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    [(my-take h a-seq) (my-drop h a-seq)]))

(defn seq-merge [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) '()
   (and (empty? seq1) (not (empty? seq2))) seq2
   (and (not (empty? seq1)) (empty? seq2)) seq1
   :else (let [f1 (first seq1)
               f2 (first seq2)]
           (if (< f1 f2)
             (cons f1 (seq-merge (rest seq1) seq2))
             (cons f2 (seq-merge seq1 (rest seq2)))))))


(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[le ri] (halve a-seq)]
      (seq-merge (merge-sort le) (merge-sort ri)))))




(defn half-monotonic-helper [dir seq1 seq2]
  (cond
   (empty? seq2) [seq1 seq2]
   (empty? seq1) (half-monotonic-helper dir (seq [(first seq2)]) (rest seq2))
   :else (let [le (last seq1)
               ri (first seq2)
               new-dir (compare le ri)
               go-on (or (nil? dir) (= dir new-dir))]
           (if go-on
             (half-monotonic-helper new-dir (concat seq1 [ri]) (rest seq2))
             (seq [seq1 seq2])))))


(defn half-monotonic
  "Takes a sequence and splits it into two sequences the first one being monotonic.
  It doesn't matter if the direction is up or down"
  [a-seq]
  (if (empty? a-seq)
    '(())
    (half-monotonic-helper nil [] a-seq)))


(defn split-into-monotonics [a-seq]
  (let [[le ri] (half-monotonic a-seq)]
    (if (or (nil? ri) (empty? ri))
      [le]
      (cons le (split-into-monotonics ri)))))


(defn permute-base-helper [n el coll]
  (if (= n 0)
    [(cons el coll)]
    (let [[le ri] (split-at n coll)]
      (cons (concat le [el] ri)
            (permute-base-helper (dec n) el coll)))))


(defn permute-base
  "Takes an element and a collection and returns a collection of collections formed by
  inserting the new element in all the possible positions of the given collection"
  [n coll]
  (permute-base-helper (count coll) n coll))


(defn permutations
  "Take a collection and returns the permutations of the given collection (n!)"
  [a-set]
  (if (empty? a-set)
    '(())
    (let [fi           (first a-set)
          rest-permute (permutations (rest a-set))]
      (if (empty? rest-permute)
        (seq ['(fi)])
        (apply concat (map (fn [c] (permute-base fi c)) rest-permute))))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [fi (first a-set)
          re (rest a-set)]
      (set
             (concat (powerset re)
                   (map (fn [x] (clojure.set/union #{fi} x))
                        (powerset re)))))))




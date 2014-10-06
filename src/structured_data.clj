(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[n z] [(get v 0)(get v 2)]]
    (+ n z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (- x2 x1) (- y2 y1))
      true
      false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1)(- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (and (<= (get point 0) x2) (>= (get point 0) x1)
             (<= (get point 1) y2) (>= (get point 1) y1))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer]
    (let [[[a1 b1][a2 b2]] inner]
    (if (and (<= x1 a1) (>= x2 a2 ) (<= y1 b1)(>= y2 b2))
      true
      false))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
   (if (> (count (get book :authors)) 1)
  true
  false))

(defn add-author [book new-author]
  (let [n (:authors book)]
        (assoc book :authors (conj n new-author))))

(defn alive? [author]
  (if (contains? author :death-year)
      false
      true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [m (fn [y] (get y 1))]
    (map m collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [c (set a-seq)]
    (if (< (count c) (count a-seq))
      true
      false)))

(defn old-book->new-book [book]
  (assoc book :authors (set(:authors book))))

(defn has-author? [book author]
  (let [o (set(:authors book))]
    (contains? o author)))

(defn authors [books]
  (clojure.set/union (set (apply concat(map :authors books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (cond
   (contains? author :death-year) (str (:name author) " (" (str(:birth-year author))
                                       " - " (str (:death-year author)) ")")
   (and (= (contains? author :death-year) false) (contains? author :birth-year))
      (str (:name author) " (" (str (:birth-year author)) " - )")
   :else (str (:name author))
   ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
   (= (count books) 1) (str (count books) " book. " (apply str (map book->string books)) ".")
    (> (count books) 1) (str (count books) " books. " (apply str (interpose ", " (map book->string books))) ".")
   :else (str "No books.")
    ))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (if (empty? (filter #(alive? %) (:authors book)))
    false
    true))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%

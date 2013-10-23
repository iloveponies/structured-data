(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    ( - x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    ( - y2 y1)))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
    (if (and (<= x1 x x2) (<= y1 y y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (if (and (contains-point? outer point1) (contains-point? outer point2))
      true
      false)))

(defn title-length [book]
  (count (book :titel)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book))
    true
    false))

(defn add-author [book new-author]
  (let [coll-of-authors (conj (:authors book) new-author)]
    (assoc book :authors coll-of-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-value (fn [x] (get x 1))]
    (map get-value collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply <= a-seq) true
   (apply >= a-seq) true
   :else false))

(defn stars [n]
  (apply concat (repeat n "*")))

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%

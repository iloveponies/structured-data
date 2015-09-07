(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[o _ O]]
  (+ o O))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [a b]]
  (and (<= x1 a x2)
       (<= y1 b y2)))

(defn contains-rectangle? [outer [p1 p2]]
  (and (contains-point? outer p1)
       (contains-point? outer p2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-el (fn [x] (get x 1))]
    (map second-el collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

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

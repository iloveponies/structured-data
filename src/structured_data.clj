(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)] (Math/pow x2 x2)))

(defn spiff [v]
  (let [v1 (get v 0)
        v3 (get v 2)]
    (+ (if v1 v1 0) (if v3 v3 0))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z]]
  (+ x z))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        res (- x1 x2)]
    (if (< res 0) (- 0 res) res)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        res (- y1 y2)]
    (if (< res 0) (- 0 res) res)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x3 y3]] rectangle
        [x2 y2] point]
    (and (<= x1 x2 x3) (<= y1 y2 y3))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left) (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-of (fn [x] (get x 1))]
    (map second-of collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [years
        (if (:birth-year author)
          (str " (" (:birth-year author) " - " (:death-year author) ")")
          "")]
    (str (:name author) years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (cond
                    (== (count books) 0) "No books"
                    (== (count books) 1) "1 book. "
                    :else (str (count books) " books. "))]
    (str book-count (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (if (empty? (filter (fn [x] (alive? x)) (:authors  book))) false true))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

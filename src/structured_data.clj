(ns structured-data)

(defn do-a-thing [x]
  (let [t (+ x x)]
    (Math/pow t t)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v0 v1 v2] v]
    (+ v0 v2)))

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
  (let [[w h] [(width rectangle) (height rectangle)]]
    (== w h)))

(defn area [rectangle]
  (let [[w h] [(width rectangle) (height rectangle)]]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py]              point]
    (and
     (<= x1 px x2)
     (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and
     (contains-point? outer point1)
     (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [not-added (:authors book)
        added     (conj not-added new-author)]
    (assoc book :authors added)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second-element (fn [elem] (get elem 1))]
    (map get-second-element collection)))

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
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [get-authors (fn [book] (:authors book))]
    (apply clojure.set/union (map get-authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [cruft1 (str  " (")
        cruft2 (str " - ")
        cruft3 (str   ")")]
    (cond
     (:death-year author) (str (:name author) cruft1 (:birth-year author) cruft2 (:death-year author) cruft3)
     (:birth-year author) (str (:name author) cruft1 (:birth-year author) cruft2 cruft3)
     :else (str (:name author)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [written-by (str ", written by ")]
    (str (:title book) written-by (authors->string (:authors book)))))

(defn books->string [books]
  (let [lost-trees (count books)
        fancy (str (apply str (interpose ". " (map book->string books))) (str "."))]
    (cond
     (== 0 lost-trees) (str "No books.")
     (== 1 lost-trees) (str (str "1 book. " ) fancy)
     :else (str (count books) (str " books. ") fancy))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [is-name? (fn [author] (= name (str (:name author))))
        filtered (filter is-name? authors)]
    (if (empty? filtered) nil (first filtered))))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

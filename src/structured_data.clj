(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

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
  (if (== 0 (- (width rectangle) (height rectangle)))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
    [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1)
         (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [coll] (get coll 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not
    (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [author-names (:authors book)]
    (assoc book :authors (set author-names))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-names
        (fn [book] (:authors book))]
    (apply clojure.set/union (map author-names books))))

(defn all-author-names [books]
  (let [author-name
        (fn [author] (:name author))]
    (set (map author-name (authors books)))))

(defn author->string [author]
  (let [author-name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (cond
      (contains? author :death-year)
        (str author-name " (" birth " - " death ")")
      (contains? author :birth-year)
        (str author-name " (" birth " - )")
    :else (str author-name))))

(defn authors->string [authors]
  (apply str
         (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (empty? books)
      (str "No books.")
    (== 1 (count books))
      (str "1 book. " (book->string (get books 0)) ".")
    :else
      (str (apply str (count books) " books. "
           (interpose ". " (map book->string books))) ".")))


(defn books-by-author [author books]
  (let [book-has-author
        (fn [book] (has-author? book author))]
    (filter book-has-author books)))

(defn author-by-name [name authors]
 (let [equal-name
       (fn [author] (= (:name author) name))]
   (first (filter equal-name authors))))

(defn living-authors [authors]
  (let [living
       (fn [author] (alive? author))]
    (filter living authors)))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (let [book-with-living-author
         (fn [book] (has-a-living-author? book))]
    (filter book-with-living-author books)))

; %________%

(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 2) (get v 0)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- (max y1 y2) (min y1 y2))))

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- (max x1 x2) (min x1 x2))))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (even? (+ x1 x2 y1 y2))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- y1 y2) (- x1 x2))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[pX pY] point]
      (and (and (>= pX x1) (<= pX x2)) (and (>= pY y1) (<= pY y2))))))

(defn contains-rectangle? [outer inner]
  (let [[pointA pointB] inner]
    (and (contains-point? outer pointA) (contains-point? outer pointB))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [original (conj (:authors book) new-author)]
    (assoc book :authors original)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getsec (fn [col] (get col 1))]
    (map getsec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (distinct a-seq))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [union (fn [x] (:authors x))]
    (set (apply clojure.set/union (map union books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)]
    (let [birth-year (:birth-year author)
          death-year (:death-year author)]
      (if (= birth-year nil) (str name) (str name " (" birth-year " - " death-year ")")))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count-str (str (count books))]
    (case (count books)
      0 (str "No books.")
      1 (str "1 book. " (apply str (interpose ", " (map book->string books))) ".")
      (str book-count-str " books. " (apply str (interpose ", " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [living-authors (living-authors (:authors book))]
    (if (empty? living-authors) false true)))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%

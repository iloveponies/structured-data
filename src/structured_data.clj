(ns structured-data)

(defn abs [x]
  (if (< x 0) (- x) x))

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (if (>= (count v) 3)
    (+ (get v 0)
      (get v 2))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a _ c]]
  (if (and a c)
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (abs (- x1 x2))))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (abs (- y1 y2))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
       [x y] point]
    (if (and (<= x1 x x2) (<= y1 y y2)) true false)))

(defn contains-rectangle? [outer [point1 point2]]
  (if
    (and
      (contains-point? outer point1)
      (contains-point? outer point2)) true false))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors-new (conj (:authors book) new-author)]
    (assoc book :authors authors-new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))

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
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)
        bd (str " (" birth " - " death ")")]
    (str name (if birth bd))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string (:authors book))))

(defn book-count-sentence [books]
  (case (count books)
    0 "No books."
    1 "1 book."
    (str (count books) " books.")))

(defn books->string [books]
  (apply str (interpose " " (cons (book-count-sentence books)
                                  (map (fn [x] (str x "."))
                                       (map book->string books))))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [f (fn [x] (= name (:name x)))]
    (first (filter f authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

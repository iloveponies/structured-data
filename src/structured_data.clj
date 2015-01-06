(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x _ y]]
  (+ x y))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[leftX] [rightX]]]
  (- rightX leftX))

(defn height [[[_ bottomY] [_ topY]]]
  (- topY bottomY))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [r]
  (*(width r) (height r)))

(defn contains-point? [[[bottomX bottomY] [topX topY]] [x y]]
  (and
    (<= bottomX x topX)
    (<= bottomY y topY)))

(defn contains-rectangle? [rect [bottom-left top-right]]
  (and
    (contains-point? rect bottom-left)
    (contains-point? rect top-right)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [element] (count element)) collection))

(defn second-elements [collection]
  (let [get-second (fn [vector] (get vector 1))]
     (map get-second collection)))

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
  (let [old-count (count a-seq)]
    (not (== old-count (count (set a-seq))))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (clojure.set/union (set (map :name (authors books)))))

(defn author->string [author]
  (let [name (:name author)
        years (str " (" (:birth-year author) " - " (:death-year author) ")")]
    (str name (if (:birth-year author) years nil))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)]
    (if (== book-count 0)
    "No books."
    (str book-count
         (if (> book-count 1) " books. " " book. ")
         (apply str (interpose ". " (map book->string books)))
         "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (if (<= x1 x2)
         (- x2 x1)
         (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (if (<= y1 y2)
         (- y2 y1)
         (- y1 y2))))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (* (width rectangle) (height rectangle))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [z1 z2] point]
    do (and (<= x1 z1 x2) (<= y1 z2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    do (and (contains-point? outer (point x1 y1))
            (contains-point? outer (point x1 y2))
            (contains-point? outer (point x2 y1))
            (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [author-list     (:authors book)
        authors-changed (conj author-list new-author)]
    do (assoc book :authors authors-changed)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
    (map count collection))

(defn second-elements [collection]
  (let [extract-second (fn [a-list] (get a-list 1))]
  (map extract-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [list-count (count a-seq)
        set-count  (count (set a-seq))]
    (not (== list-count set-count))))

(defn old-book->new-book [book]
  (let [authors-set (set (:authors book))]
    (assoc book :authors authors-set)))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (let [author-names (map :authors books)]
    (apply clojure.set/union author-names)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nam (:name author)
        death-year (:death-year author)
        birth-year (:birth-year author)]
    (if (nil? birth-year)
       (str nam)
       (str nam " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
  (let [author-names (map author->string authors)]
    (apply str (interpose ", " author-names))))

(defn book->string [book]
  (let [book-name (:title book)
        names (authors->string (:authors book))]
    (apply str [book-name ", written by " names])))

(defn books->string [books]
  (let [bnum         (count books)
        book-strings (map book->string books)
        book-list    (clojure.string/join ". " book-strings)
        num-string   (cond
                       (zero? bnum) "No books"
                       (= bnum 1)   "1 book. "
                       :else        (str bnum " books. "))]
     (str num-string (clojure.string/join ". " [book-list]) ".")))

(defn books-by-author [author books]
  (let [author-pred (fn [book] (has-author? book author))]
    (filter author-pred books)))

(defn author-by-name [name authors]
  (let [name-matcher (fn [x] (= (:name x) name))]
    (first (filter name-matcher authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [living (living-authors (:authors book))]
    (not (empty? living))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

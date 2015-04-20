(ns structured-data)

(defn do-a-thing [x]
  (let [xplusx (+ x x)]
        (Math/pow xplusx xplusx)))

(defn spiff [v] (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (spiff v))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[k1 k2] inner]
    (and (contains-point? outer k1) (contains-point? outer k2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [old (get book :authors)
        new (conj old new-author)]
    (assoc book :authors new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second_2th (fn [x] (get x 1))]
    (map second_2th collection)))

(defn titles [books]
    (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if(contains? a-set elem)
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
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply clojure.set/union (map author-names books)))))

(defn author->string [author]
  (let [birth-year (:birth-year author)
        death-year (:death-year author)
        str_year (if birth-year
                   (str " (" birth-year " - " death-year ")") "")]
    (str (:name author) str_year)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-title (:title book)]
    (str book-title ", written by " (authors->string (:authors book)))))

(defn books->string [books]
  (let [b-num (count books)
        text (str (apply str (interpose ". "(map book->string books))) ".")
        ]
    (if (= b-num 0) "No books." (if (= b-num 1) (str "1 book. " text) (str b-num " books. " text)))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (boolean (seq (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%

(ns structured-data)

(defn do-a-thing [x]
  (let [x (+ x x)]
    (Math/pow x x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (let [love "<3"] (conj v love)))

(defn spiff-destructuring [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [a (get rectangle 0)
        b (get rectangle 1)]
    (- (get b 0) (get a 0))))

(defn height [rectangle]
  (let [a (get rectangle 0)
        b (get rectangle 1)]
    (- (get b 1) (get a 1))))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle)) true false))

(defn contains-point? [[[left bottom] [right top]] [x y]]
  (and (<= left x right)
       (<= bottom y top)))

(defn contains-point? [rectangle point]
  (let [x1 (get (get rectangle 0) 0)
        x2 (get (get rectangle 1) 0)
        y1 (get (get rectangle 0) 1)
        y2 (get (get rectangle 1) 1)
        x  (get point 0)
        y  (get point 1)]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [a (get inner 0)
        b (get inner 1)] 
    (and (contains-point? outer a) (contains-point? outer b))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count(:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [a (fn [v] (get v 1))]
    (map a collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

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
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name-str (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        year-str (str " (" birth-year " - " death-year ")")]
    (str name-str (if birth-year year-str))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count->string (fn [book-count] (str book-count " book" (if (> book-count 1) "s") "."))
        book-count-str (book-count->string (count books))
        books-str (apply str (interpose ", " (map book->string books)))]
    (cond 
      (== (count books) 0) "No books."
      :else (str book-count-str " " books-str "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
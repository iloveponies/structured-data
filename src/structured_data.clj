(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v0 v1 v2 v3] v]
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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn is-between? [z x y]
  (<= x z y))

(prn "is-between " 2 3 4 (is-between? 2 1 5))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point] 
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1) 
         (contains-point? outer point2) )))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
 (count (:authors book)))

(defn multiple-authors? [book]
 (> (author-count book) 1 ))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
    (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [rest-collection (map rest collection)]
    (map first rest-collection)))

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
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [book-authors (:authors book)] 
    (assoc book :authors (set book-authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
 (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [book-authors
        (fn [book] (:authors book))
        book-authors-names
        (fn [book] (map :name (book-authors book)))]
    (set (apply clojure.set/union (map book-authors-names books))) ))

(defn author->string [author]
  (let [b-year (if (contains? author :birth-year) (str " (" (:birth-year author) " - ") nil)
        e-year (if (contains? author :death-year) (str      (:death-year author)) nil)
        e-year2 (if (nil? b-year) nil (str e-year ")"))
        name (:name author)]
    (str name b-year e-year2)))

(defn authors->string [authors]
  (let [names (apply str (interpose ", " (map author->string authors)))]
    names))

(defn book->string [book]
  (let [authors (authors->string (:authors book))
        name (:title book)]
    (apply str [name ", written by " authors])))

(defn books->string [books]
  (let [book-authors (apply str (interpose ". " (map book->string books)))
        books-count (count books)
        book-count-str (case books-count
                             0 "No books"
                             1 "1 book. " 
                             (str books-count " books. "))]
    (str book-count-str book-authors ".")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors))) 

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [live-authors (living-authors (:authors book))]
    (not (empty? live-authors))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

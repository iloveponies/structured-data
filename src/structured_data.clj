(ns structured-data)

(defn do-a-thing [x]
  (let [p (+ x x)]
    (Math/pow p p)))

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
    [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    (and (contains-point? outer bl) (contains-point? outer tr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn stars [n]
  (apply str (repeat n "*")))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (let [newauthors (:authors book)]
    (assoc book :authors (set newauthors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
       by (:birth-year author)
       dy (:death-year author)]
    (if (boolean by)
    (str name " (" by " - " dy ")")
    (str name))))

(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))

(defn book->string [book]
  (let [book-title (:title book)
        authors (authors->string (:authors book))]
    (str book-title ", written by " authors)))

(defn books->string [books]
  (let [nbooks (count books)
        book-word (if (= nbooks 1) " book. " " books. ")
        books-seq (map book->string books)]

  (if (empty? books)
    (str "No books.")
    (str nbooks book-word (apply str (interpose ", " books-seq)) "."))
  ))

(defn books-by-author [author books]
  (let [has-this-author? (fn [book] (has-author? book author))]
  (filter has-this-author? books)))

(defn author-by-name [name authors]
  (let [result (set (map :name authors))
        has-this-name? (fn [author](= (:name author) name))]
    (if (contains? result name)
      (first (filter has-this-name? authors))
      nil)
    ))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
  (not (empty? (filter alive? authors)))
    ))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

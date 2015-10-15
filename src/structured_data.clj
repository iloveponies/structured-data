(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx))
)



(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y))
  )

(defn cutify [v]
(conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1))
  )

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
      [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2)))
  )

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
  (and (contains-point? outer point1)
      (contains-point? outer point2)))
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (< 1 (author-count book))
  )

(defn add-author [book new-author]
(let [newauthors (conj (:authors book) new-author)]
  (assoc book :authors newauthors))
  )

(defn alive? [author]
  (not(contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection))
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (let [setified (set a-seq)]
  (< (count setified) (count a-seq)))
  )

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
  (assoc book :authors authors))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (:name author)
        years (if (contains? author :birth-year)(str " (" (:birth-year author) " - " (:death-year author) ")"))]
  (str name years))
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [name (:title book)
        authors (authors->string (:authors book))]
    (str name ", written by " authors))
  )

(defn books->string [books]
  (let [amount (count books)
        book-a (fn [am] (cond
                         (== am 0) "No books"
                         (== am 1) "1 book. "
                         :else (str am " books. ")))
        book-n (fn [am] (if (< 0 am)
                          (apply str (interpose ". "(map book->string books)))))]
    (str (book-a amount) (book-n amount) ".")
  ))

(defn books-by-author [author books]
  (let [filterer (fn [book] (has-author? book author))]
    (filter filterer books))
  )

(defn author-by-name [name authors]
  (let [filterer (fn [n] (= (:name n) name))]
    (first (filter filterer authors)))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%

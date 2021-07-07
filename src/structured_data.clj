(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first-index second-index third-index] v]
    (+ first-index third-index)))

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
        [point-x point-y] point]
    (and
      (<= x1 point-x x2)
      (<= y1 point-y y2))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and
      (contains-point? outer inner-bottom-left)
      (contains-point? outer inner-top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (>= (author-count book) 2))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [c] (get c 1))]
    (map second-element collection)))

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
    (=
      (count a-seq)
      (count (set a-seq)))))

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
        years (if (:birth-year author)
                (str "(" (:birth-year author) " - " (:death-year author) ")")
                "")]
    (if (empty? years)
      name
      (str name " " years))))

(defn authors->string [authors]
  (let [authors-as-strings (map author->string authors)]
    (apply str (interpose ", " authors-as-strings))))

(defn book->string [book]
  (let [book-title (:title book)
        author-string (authors->string (:authors book))]
    (str book-title ", written by " author-string)))

(defn books->string [books]
  (let [number-of-books
          (if (zero? (count books))
            "No books"
            (str (count books) " book" (if (>= (count books) 2) "s" "")))
        books-as-strings (map book->string books)
        books-as-string (apply str (interpose ". " books-as-strings))]
    (str
      number-of-books
      "."
      (if (empty? books-as-string) "" (str " " books-as-string ".")))))

(defn books-by-author [author books]
  (let [by-author? (fn [book] (contains? (:authors book) author))]
    (filter by-author? books)))

(defn author-by-name [name authors]
  (let [has-given-name? (fn [author] (= name (:name author)))]
    (first (filter has-given-name? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

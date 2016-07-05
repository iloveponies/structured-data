(ns structured-data)

(defn do-a-thing [x]
  (let [twoX (+ x x)]
    (Math/pow twoX twoX)))

(defn spiff [v]
  (+ (get v 0 0) (get v 2 0)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[first second third]]
  (+ first third))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[bottom-left-x bottom-left-y][top-right-x top-right-y]]]
  (- top-right-x bottom-left-x))

(defn height [[[bottom-left-x bottom-left-y][top-right-x top-right-y]]]
  (- top-right-y bottom-left-y))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[bottom-left-x bottom-left-y][top-right-x top-right-y]] rectangle
        [x y] point
       ]
       (and (<= bottom-left-x x top-right-x) (<= bottom-left-y y top-right-y))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and (contains-point? outer inner-bottom-left) (contains-point? outer inner-top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [c] (get c 1))]
    (map second collection)))

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
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (str name
      (if birth-year
        (str " (" birth-year " - " (if death-year death-year "") ")")))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (let [book_count (count books)
        all_books (apply str (interpose ". " (map book->string books)))]
    (cond
      (== 0 book_count) "No books."
      (== 1 book_count) (str "1 book. " all_books ".")
      :else (str book_count " books. " all_books "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

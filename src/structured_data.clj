(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[f _ t] v]
    (+ f t)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[blX blY] [trX trY]] rectangle]
    (- trX blX)))

(defn height [rectangle]
  (let [[[blX blY] [trX trY]] rectangle]
    (- trY blY)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[blX blY] [trX trY]] rectangle
        [x y] point
        ]
    (and (<= blX x trX) (<= blY y trY))))

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
  (let [authors (:authors book)
        new-authors (conj authors new-author)
        updated-book (assoc book :authors new-authors)]
    updated-book))

(defn alive? [author]
  (let [dead? (contains? author :death-year)]
    (not dead?)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [increasing? (apply >= a-seq)
        decreasing? (apply <= a-seq)]
    (or increasing? decreasing?)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)
        a-set-count (count a-set)
        a-seq-count (count a-seq)]
    (not= a-set-count a-seq-count)))

(defn old-book->new-book [book]
  (let [authors (:authors book)
        new-authors (set authors)
        updated-book (assoc book :authors new-authors)]
    updated-book))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (let [books-authors (map :authors books)]
    (apply clojure.set/union books-authors)))

(defn all-author-names [books]
  (let [all-authors (authors books)
        all-names (map :name all-authors)]
    (set all-names)))

(defn author->string [author]
  (let [year? (contains? author :birth-year)
        birth (:birth-year author)
        death (or (:death-year author) "")
        years (if year? (str " (" birth " - " death ")") "")
        name (:name author)
        author-string (str name years)]
    author-string))

(defn authors->string [authors]
  (let [author-strings (map author->string authors)
        with-sepparator (interpose ", " author-strings)
        authors-string (apply str with-sepparator)]
    authors-string))

(defn book->string [book]
  (let [title (:title book)
        by (authors->string (:authors book))]
    (str title ", written by " by)))

(defn books->string [books]
  (let [books-count (count books)
        books-count-string (cond
                             (empty? books) "No books."
                             (== 1 books-count) "1 book."
                             :else (str books-count " books."))
        books-string (if (empty? books) ""
                       (str " " (apply str (interpose ". " (map book->string books))) "."))]
    (str books-count-string books-string)))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [living (living-authors (:authors book))]
    (not (empty? living))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

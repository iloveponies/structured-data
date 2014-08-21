(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[bottom-left-x bottom-left-y] [top-right-x top-right-y]] rectangle]
        (- top-right-x bottom-left-x)))

(defn height [rectangle]
  (let [[[bottom-left-x bottom-left-y] [top-right-x top-right-y]] rectangle]
    (- top-right-y bottom-left-y)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[bottom-left-x bottom-left-y] [top-right-x top-right-y]] rectangle
        [px py] point]
    (and
     (<= bottom-left-x px top-right-x)
     (<= bottom-left-y py top-right-y))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left-inner top-right-inner] inner]
    (and
     (contains-point? outer bottom-left-inner)
     (contains-point? outer top-right-inner))
    ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (let [{authors :authors} book
        empty-or-existing (if authors authors {})
        new-authors (conj empty-or-existing new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [v] (get v 1))]
    (map sec collection)))

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
  (let [seq-length (count a-seq)
        set-length (count (set a-seq))]
    (not= seq-length set-length)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (let [{authors :authors} book]
    (contains? authors author)))

(defn authors [books]
  (let [get-authors (fn [book] (:authors book))
        authors-set (map get-authors books)]
    (apply clojure.set/union authors-set)))

(defn all-author-names [books]
  (let [authors-set (authors books)
        authors-seq (seq authors-set)]
    (set (map :name authors-seq))))

(defn author->year [author]
  (if (contains? author :birth-year)
    (str "(" (:birth-year author) " - " (:death-year author) ")")
    ""))

(defn author->string [author]
  (let [years (author->year author)
        year-suffix (str (if (= "" years) "" " ") years)]
    (str (:name author) year-suffix)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-authors (authors->string (:authors book))
        book-string (if (= "" book-authors) (:title book) (str (:title book) ", written by " book-authors))]
    book-string))

(defn books->string [books]
  (let [book-count (count books)
        book-count-string (cond
                           (= 0 book-count) "No books"
                           (= 1 book-count) "1 book"
                           :else (str book-count " books"))
        all-books (cons book-count-string (map book->string books))
        all-books-delimited (interpose ". " all-books)
        all-books-string (apply str (conj (vec all-books-delimited) "."))]
    all-books-string))

(defn books-by-author [author books]
  (let [book-has-author? (fn [bk] (has-author? bk author))]
    (filter book-has-author? books)))

(defn author-by-name [name authors]
  (let [matching-authors (filter (fn [auth] (= name (:name auth))) authors)]
    (if (= 0 (count matching-authors))
      nil
      (first matching-authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [book-authors (:authors book)
        book-living-authors (living-authors book-authors)]
    (not (empty? book-living-authors))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

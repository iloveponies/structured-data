(ns structured-data)

(defn do-a-thing [x]
  (let [twox (+ x x)]
    (Math/pow twox twox)))

(defn spiff [v]
  (+
    (get v 0)
    (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))

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
  (=
    (height rectangle)
    (width rectangle)))

(defn area [rectangle]
  (*
    (height rectangle)
    (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [x y] point]
    (and
      (<= x1 x x2)
      (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and
      (contains-point? outer bottom-left)
      (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

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
  (not=
    (count a-seq)
    (count (set a-seq))))

(defn old-book->new-book [book]
  (let [old-authors (:authors book)
        new-authors (set old-authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors (map :authors books)]
    (apply clojure.set/union authors)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if birth
      (str name " (" birth " - " death ")")
      name)))

(defn authors->string [authors]
  (apply
    str
    (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-str (:title book)
        authors-string (authors->string (:authors book))]
    (str book-str ", written by " authors-string)))

(defn books->string [books]
  (let [count (count books)
        book-strings (map book->string books)
        book-string-parts (interpose ". " book-strings)
        book-string (apply str book-string-parts)]
    (cond
      (= 0 count) (str "No books.")
      (= 1 count) (str "1 book. " book-string ".")
      :else (str count " books. " book-string "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [has-name? (fn [author] (= (:name author) name))]
    (first (filter has-name? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

(ns structured-data)

(defn do-a-thing [x]
  (let [x-x (+ x x)]
    (Math/pow x-x x-x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [a (get v 0)
        b (get v 2)]
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
  (let [w (width rectangle)
       h (height rectangle)]
    (= w h)))

(defn area [rectangle]
  (let [w (width rectangle)
       h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3]           point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left) (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        authors (conj authors new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
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
  (let [a-length (count a-seq)
        s-length (count (set a-seq))]
    (not= a-length s-length)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [a-name  (:name author)
        birth  (:birth-year author)
        death  (:death-year author)
        years  (str " (" birth " - " death ")")]
    (if birth
      (str a-name years)
      a-name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [tot-books (count books)
        book-str  (apply str (interpose ". " (map book->string books)))]
    (cond
      (empty? books)   "No books."
      (== 1 tot-books) (str "1 book. " book-str ".")
      :else            (str tot-books " books. " book-str "."))))

(defn books-by-author [author books]
  (let [check-author (fn [book] (has-author? book author))]
    (filter check-author books)))

(defn author-by-name [name authors]
  (let [check-name (fn [author] (= name (:name author)))]
    (first (filter check-name authors))))

(defn living-authors [authors]
    (filter alive? authors))

(defn has-a-living-author? [book]
  (if (seq (living-authors (:authors book)))
    true
    false))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

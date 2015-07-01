(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [first (get v 0)
        second (get v 2)]
    (+ first second)))

(defn cutify [v]
  (let [tail  "<3"]
    (conj v tail)))

(defn spiff-destructuring [v]
  (let [[a x b] v]
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
    (let [rHeight (height rectangle)
        rWidth (width rectangle)]
    (= rHeight rWidth)))

(defn area [rectangle]
    (let [rHeight (height rectangle)
        rWidth (width rectangle)]
    (* rHeight rWidth)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (and (<= x1 p1 x2)
        (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
        (contains-point? outer p2))))

(defn title-length [book]
  (let [title (get book :title)]
    (count title)))

(defn author-count [book]
  (let [authors (get book :authors)]
    (count authors)))

(defn multiple-authors? [book]
  (>= (author-count book) 2))

(defn add-author [book new-author]
  (let [title (get book :title)
        authors (get book :authors)]
    (if title
      {:title title :authors (conj authors new-author)}
      {:authors (conj authors new-author)})))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map #(count %) collection))

(defn second-elements [collection]
    (let [pluck-second (fn [x]
      (get x 1))]
      (map pluck-second collection)))

(defn titles [books]
  (let [pluck-title (fn [x]
    (get x :title))]
    (map pluck-title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
        (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq)
        (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        yod (:death-year author)
        yob (:birth-year author)
        year (if yob
          (str " (" yob " - " yod ")")
          "")]
    (str name year)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [bookTitle (:title book)
        authorsString (authors->string (:authors book))]
    (str bookTitle ", written by " authorsString)))

(defn books->string [books]
  (let [bookCount (count books)
        bookString (interpose ". " (map book->string books))]
    (if (<= bookCount 0)
      "No books."
      (if (= bookCount 1)
            (str "1 book. " (apply str bookString) ".")
            (str bookCount " books. " (apply str bookString) ".")))))

(defn books-by-author [author books]
  (let [has-author-map (fn [book]
    (has-author? book author))]
    (filter has-author-map books)))

(defn author-by-name [name authors]
  (let [has-name? (fn [author]
    (= name (:name author)))]
    (first (filter has-name? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)
        livingAuthors (living-authors authors)]
    (not (empty? livingAuthors))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

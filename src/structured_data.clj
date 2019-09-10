(ns structured-data)

(defn do-a-thing [x]
  (let [x-double (+ x x)]
    (Math/pow x-double x-double)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
  (let [ [[left bottom] [right top]] rectangle]
    (- right left)))

(defn height [rectangle]
  (let [ [[left bottom] [right top]] rectangle]
    (- top bottom)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[left bottom] [right top]] rectangle
        [x y] point]
    (and (<= left x right)
         (<= bottom y top))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  (count
   (:title book)))

(defn author-count [book]
  (count
   (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map #(get % 1) collection))

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
  (apply clojure.set/union
   (map :authors books)))

(defn all-author-names [books]
  (set
    (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)]
    (if-let [birth (:birth-year author)]
      (if-let [death (:death-year author)]
        (str name " (" birth " - " death ")")
        (str name " (" birth " - )"))
      name)))

(defn authors->string [authors]
  (apply str
    (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str
   (:title book)
   ", written by "
   (authors->string (:authors book))))

(defn books->string [books]
    (let [count-string (case (count books)
                         0 "No books."
                         1 "1 book. "
                         (str (count books) " books. "))
          book-strings (map #(str (book->string %) ".") books)
          book-string (apply str (interpose " " book-strings))]
      (str count-string book-string)))


(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))


(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


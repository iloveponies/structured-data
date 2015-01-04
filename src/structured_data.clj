(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [one (or (get v 0) 0)
        two (or (get v 2) 0)]
    (+ one two)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[one _ two] v]
    (+ one two)))

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
  (if (= (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[x y] point
        [[rx1 ry1] [rx2 ry2]] rectangle]
    (if (and (<= rx1 x rx2) (<= ry1 y ry2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[[r2x1 r2y1] [r2x2 r2y2]] inner]
    (if (and (contains-point? outer [r2x1 r2y1]) (contains-point? outer [r2x2 r2y2]))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map #(get % 1) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [size-seq (count a-seq)
        size-set (count (set a-seq))]
    (if (= size-seq size-set)
      false
      true)))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [authors (authors books)]
    (set (map :name authors))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)
        year (cond
               (nil? birth) ""
               (nil? death) (str " (" birth " - )")
               :else (str " (" birth " - " death ")"))]
    (str name year)))

(defn authors->string [authors]
  (let [authors-as-string (map author->string authors)]
    (apply str (interpose ", " authors-as-string))))

(defn book->string [book]
  (let [authors (authors->string (:authors book))]
    (str (:title book) ", written by " authors)))

(defn books->string [books]
  (let [number-of-books (count books)
        books-string (map book->string books)
        how-many-books (cond
                         (= 0 number-of-books) "No books"
                         (= 1 number-of-books) "1 book. "
                         :else (str number-of-books " books. "))]
    (str how-many-books (apply str (interpose ", " books-string)) ".")))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)
        living-ones (living-authors authors)]
    (if (empty? living-ones)
      false
      true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

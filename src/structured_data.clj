(ns structured-data)

(defn do-a-thing [x]
  (
   let [doublex (+ x x)]
   (Math/pow doublex doublex))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a b c]]
  (+ a c))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn dimensions [rectangle]
  ((juxt width height) rectangle))

(defn square? [rectangle]
  (apply = (dimensions rectangle)))

(defn area [rectangle]
  (apply * (dimensions rectangle)))

(defn contains-point? [bounds xs]
  (defn val-in-bounds [[[x1 x2] x]] (<= x1 x x2))
  (every? true? (map val-in-bounds (map vector (apply mapv vector bounds) xs))))

(defn contains-rectangle? [outer inner]
  (every? true? (map (partial contains-point? outer) inner)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map #(get %1 1) collection))

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
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
 (reduce clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [years (if (contains? author :birth-year)
        (str " (" (:birth-year author) " - " (:death-year author) ")")
        (str ""))]
    (str (:name author) years)))

(defn comma-separated [strings]
  (apply str (interpose ", " strings)))

(defn authors->string [authors]
 (comma-separated (map author->string authors)))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-amount
        (if (empty? books)
          (str "No books")
          (if (= 1 (count books))
            (str "1 book. ")
            (str (count books) " books. ")))]
        (str book-amount (comma-separated (map book->string books)) ".")))

(defn books-by-author [author books]
  (filter #(contains? (:authors %1) author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %1)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
   (filter has-a-living-author? books))

; %________%

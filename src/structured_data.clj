(ns structured-data)

(defn do-a-thing [x]
  (let [doubleX (+ x x)]
    (Math/pow doubleX doubleX)))

(defn spiff [v]
  (+ (get v 2)
     (get v 0)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))


(defn contains-point? [[[x1 y1] [x2 y2]] [x0 y0]]
  (and
   (<= x1 x0 x2)
   (<= y1 y0 y2)))

(defn contains-rectangle? [outer [upperLeft lowerRight]]
  (and
   (contains-point? outer upperLeft)
   (contains-point? outer lowerRight)))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))


(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (=
   a-seq
   (apply list (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [book] (:authors book)) books)))

(defn all-author-names [books]
  (set (map (fn [author] (:name author))(authors books))))

(defn author->string [author]
  (let [authorName (:name author) yearsAlive (str "(" (:birth-year author) " - " (:death-year author) ")")]
    (if (:birth-year author)
      (str authorName " " yearsAlive)
      authorName)))

(defn authors->string [authors]
  (apply str (interpose ", " (map (fn [author] (author->string author)) authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (str
   (case (count books)
    0 "No books."
    1 "1 book."
    (str (count books) " books."))
    (apply str(map (fn [book] (str " " (book->string book) ".")) books))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
)

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

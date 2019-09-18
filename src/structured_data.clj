(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (Math/abs (- x2 x1)))

(defn height [[[x1 y1] [x2 y2]]]
  (Math/abs (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [px py]]
  (and (<= x1 px x2) (<= y1 py y2)))

(defn contains-rectangle? [outer [bl tr]]
  (and (contains-point? outer bl) (contains-point? outer tr)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [col] (get col 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))


(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))


(defn authors [books]
  (let [new-books (map old-book->new-book books)
        authors-sets (map :authors new-books)]
    (apply clojure.set/union authors-sets)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        byear (:birth-year author)
        dyear (:death-year author)
        years (if (nil? byear)
                ""
                (str " (" byear " - " dyear ")"))]
    (str name years)))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by ", (authors->string (:authors book))))

(defn books->string [books]
  (let [books-str (apply str (interpose ". " (map book->string books)))]
  (case (count books)
    0 "No books."
    1 (str "1 book. " books-str ".")
    (str (count books) " books. " books-str "."))))


(defn books-by-author [author books]
  (let [has-correct-author? (fn [book] (has-author? book author))]
    (filter has-correct-author? books)))

(defn author-by-name [name authors]
  (let [correct-name? (fn [author] (= (:name author) name))]
    (first (filter correct-name? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

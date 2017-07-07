(ns structured-data)

(defn do-a-thing [x]
  (let [th (+ x x)]
    (Math/pow th th)))

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
  (Math/abs (- x1 x2)))

(defn height [[[x1 y1] [x2 y2]]]
  (Math/abs (- y1 y2)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [a b]]
  (and
    (or (<= x1 a x2) (>= x1 a x2))
    (or (<= y1 b y2) (>= y1 b y2))))

(defn contains-rectangle? [rectangle [p1 p2]]
  (and 
    (contains-point? rectangle p1)
    (contains-point? rectangle p2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [hl (fn [x] (get x 1))]
    (map hl collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or 
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [st (set a-seq)]
    (< (count st) (count a-seq))))

(defn old-book->new-book [book]
  (let [st (set (:authors book))]
    (assoc book :authors st)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (if (contains? author :birth-year)
    (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (:name author)))

(defn authors->string [authors]
  (clojure.string/join ", " (map author->string authors)))

(defn book->string [book]
  (if (contains? book :authors)
    (str (:title book) ", written by " (authors->string (:authors book)))
    (:title book)))

(defn books->string [books]
  (let [book-count 
        (cond 
          (== (count books) 0) "No books"
          (== (count books) 1) "1 book. "
          :else (str (count books) " books. "))]
    (str book-count (clojure.string/join ". " (map book->string books)) ".")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

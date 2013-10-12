(ns structured-data)

(defn do-a-thing [x]
  (let [x (+ x x)]
    (Math/pow x x)
    ))

(defn spiff [v]
  (+ (get v 0) (get v 2)
     ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle [pointx pointy] point]
    (and (<= x1 pointx x2) (<= y1 pointy y2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (let [title (fn [x] (:title x))]
    (map title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not(== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [auth (fn [book] (:authors book))]
    (apply clojure.set/union (map auth books))))

(defn all-author-names [books]
  (let [namen (fn [author] (:name author))]
    (set (map namen (authors books)))))

(defn author->string [author]
  (let [name (str (:name author))
        year (cond
              (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
              (contains? author :birth-year) (str " (" (:birth-year author) " - )")
              :else (str ""))]
    (str name year )))

(defn authors->string [authors]
    (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [pre (cond
             (empty? books) "No books"
             (== 1 (count books)) "1 book. "
             :else (str (count books) " books. "))]
    (str pre (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (let [has-auth (fn [book] (has-author? book author))]
    (filter has-auth books)))

(defn author-by-name [name authors]
  (let [is-auth? (fn [author] (= name (:name author)))]
    (first (filter is-auth? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

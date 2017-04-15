(ns structured-data)

(defn do-a-thing [x]
  (let [X (+ x x)]
    (Math/pow X X)))

(defn spiff [v]
  (+ (get v 0)
    (get v 2)))


(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x2 x1) (- y2 y1))))


(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[z1 w1] [z2 w2]] inner]
    (and (contains-point? outer (point z1 w1))
         (contains-point? outer (point z2 w2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [scnd (fn [x] (get x 1))]
      (map scnd collection)))

(defn titles [books]
  (map :title books))

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
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply clojure.set/union (map author-names books)))))

(defn author->string [author]
  (cond
    (contains? author :death-year)
      (str (:name author) " ("
           (:birth-year author) " - "
           (:death-year author) ")")
    (contains? author :birth-year)
      (str (author :name) " ("
           (:birth-year author) " - )")
    :else (str (author :name))))

(defn authors->string [authors]
  (let [authstring (fn [author] (author->string author))]
    (apply str (interpose ", " (map authstring authors)))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookstring (fn [book] (book->string book))]
    (cond
      (< 1 (count books))
       (str (count books) " books. " (apply str (interpose ". " (map bookstring books))) ".")
      (= 1 (count books))
       (str "1 book. " (apply str (interpose ". " (map bookstring books))) ".")
      :else (str "No books."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (author :name))) authors)))

(defn living-authors [authors]
  (let [is-living? (fn [author] (not (boolean (:death-year author))))]
    (filter is-living? authors)))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

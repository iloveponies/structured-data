(ns structured-data)

(defn do-a-thing [x]
  (let [addition (+ x x)]
    (Math/pow addition addition)))

(defn spiff [v]
  (+ (get v 0)(get v 2)))

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
    (if (< x1 x2)
      (- x2 x1)
      (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (< y1 y2)
      (- y2 y1)
      (- y1 y2))))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[rx1 ry1] [rx2 ry2]] rectangle
        [px1 py1] point]
    (and (<= rx1 px1 rx2) (<= ry1 py1 ry2))))

(defn contains-rectangle? [outer inner]
  (let [[ip1 ip2] inner]
    (and (contains-point? outer ip1) (contains-point? outer ip2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book))
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [coll] (get coll 1))]
    (map second-element collection)))

(defn titles [books]
  (let [title (fn [book] (:title book))]
  (map title books)))

(defn monotonic? [a-seq]
  (cond
   (apply <= a-seq) true
   (apply >= a-seq) true
   :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count (set a-seq))(count a-seq))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))

(defn authors [books]
  (let [author (fn [book] (:authors book))]
    (apply clojure.set/union (map author books))))

(defn all-author-names [books]
  (let [author-name (fn [author] (:name author))]
    (set (map author-name (authors books)))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (cond
     death-year (str name " (" birth-year " - " death-year ")")
     birth-year (str name " (" birth-year " - )")
     :else name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (interpose ", written by " [(:title book) (authors->string (:authors book))])))

(defn books->string [books]
  (let [authors (apply str (interpose ". " (map book->string books)))
        book-count (count books)]
  (cond
   (= 0 book-count) "No books."
   (= 1 book-count) (str book-count " book. " authors ".")
   (< 1 book-count) (str book-count " books. " authors ".")
   :else nil)))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (if (= (:name author) name) author nil)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true))

(defn books-by-living-authors [books]
  (filter (fn [book] (if (has-a-living-author? book) book false)) books))

; %________%

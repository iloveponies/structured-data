(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first second third rest] v]
    (+ first third)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[lbx lby] [rtx rty]] rectangle]
    (- rtx lbx)))

(defn height [rectangle]
  (let [[[lbx lby] [rtx rty]] rectangle]
    (- rty lby)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[px py] point
        [[lbx lby] [rtx rty]] rectangle]
    (and
      (<= lbx px rtx)
      (<= lby py rty))))

(defn contains-rectangle? [outer inner]
  (let [[ibl itr] inner]
    (and
      (contains-point? outer ibl)
      (contains-point? outer itr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [original-authors (:authors book)
        new-authors (conj original-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))

(defn titles [books]
  (let [get-title (fn [x] (:title x))]
    (map get-title books)))

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
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [original-authors (:authors book)
        new-authors (set original-authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (str name
      (if birth-year
        (str " (" birth-year " - " death-year ")")))))

(defn authors->string [authors]
  (apply str
    (interpose ", "
      (map author->string authors))))

(defn book->string [book]
  (let [name (:title book)
        authors (authors->string (:authors book))]
        (str name ", written by " authors)))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [books-count (count books)]
    (str
      books-count " book" (if (< 1 books-count) "s") ". "
      (apply str (interpose ". "
        (map book->string books)))
        "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
